# Exercise from Ruby Quiz: http://rubyquiz.com/quiz155.html
# Borrowed heavily from TreeTop parser used in http://learnruby.com/examples/ruby-quiz-155.shtml

require "minitest"
require "minitest/autorun"
require "bundler/setup"
Bundler.require
require "pp"

require "speculation"
require "speculation/gen"
require "speculation/test"

S = Speculation
Gen = S::Gen
STest = S::Test

module JSONParser
  extend S::NamespacedSymbols

  def self.re_literal(string, conformed)
    kvs = string.each_char.each_with_index.reduce({}) { |h, (v, i)| h.merge(i => Set[v]) }
    S.constrained(S.cat(kvs), S.conformer { conformed })
  end

  def self.re_conform_val(re)
    S.constrained(re, S.conformer(&:last))
  end

  def self.conforming(re)
    raise ArgumentError unless block_given?
    S.constrained(re, S.conformer { |x| yield x })
  end

  def self.conform_key(key, re)
    conforming(re) { |hash| hash[key] }
  end

  S.def ns(:space), Set[" "]
  S.def ns(:spaces), S.zero_or_more(Set[" "])
  S.def ns(:null), re_literal("null", nil)

  S.def ns(:boolean), re_conform_val(S.alt(:true  => re_literal("true", true),
                                           :false => re_literal("false", false)))

  S.def ns(:number), re_conform_val(S.alt(:integer => ns(:integer),
                                          :float   => ns(:float)))

  S.def ns(:number), re_conform_val(S.alt(:integer => ns(:integer),
                                          :float   => ns(:float)))

  S.def ns(:digit), /\A[[:digit:]]\z/

  S.def ns(:exponent), conforming(S.cat(:pre    => Set["e", "E"],
                                        :sign   => S.zero_or_one(Set["-", "+"]),
                                        :digits => S.one_or_more(ns(:digit)))) { |m| m.values.join }

  S.def ns(:integer), conforming(S.cat(:neg      => S.zero_or_one(Set["-"]),
                                       :digits   => S.one_or_more(ns(:digit)),
                                       :exponent => S.zero_or_one(ns(:exponent)))) { |m| Integer(m.values.join) }

  S.def ns(:float), conforming(S.cat(:neg         => S.zero_or_one(Set["-"]),
                                     :digits      => S.one_or_more(ns(:digit)),
                                     :dot         => Set["."],
                                     :more_digits => S.one_or_more(ns(:digit)),
                                     :exponent    => S.zero_or_one(ns(:exponent)))) { |m| Float(m.values.join) }

  S.def ns(:string), conforming(S.cat(:open => Set['"'],
                                      :contents => S.zero_or_more(ns(:characters)),
                                      :close => Set['"'])) { |match| Array(match[:contents]).join }

  S.def ns(:characters), re_conform_val(S.alt(:escaped => ns(:escaped_character),
                                              :special => ns(:special_character),
                                              :regular => ns(:regular_character)))

  S.def ns(:escaped_character), conform_key(:val, S.cat(:escape => Set['\\'],
                                                        :val    => Set['\\', '"']))

  special_character_map = {
    "b" => "\b",
    "f" => "\f",
    "n" => "\n",
    "r" => "\r",
    "t" => "\t",
  }
  S.def ns(:special_character), conforming(S.cat(:escape => Set['\\'],
                                                 :val    => Set['b', 'f', 'n', 'r', 't'])) { |m| special_character_map.fetch(m[:val]) }

  S.def ns(:regular_character), ->(s) { !['\\', '"'].include?(s) }

  S.def ns(:empty_array), conforming(S.cat(:open   => Set["["],
                                           :spaces => ns(:spaces),
                                           :close  => Set["]"])) { [] }

  S.def ns(:non_empty_array), conform_key(:contents, S.cat(:open       => Set["["],
                                                           :space      => ns(:spaces),
                                                           :contents   => ns(:value_list),
                                                           :more_space => ns(:spaces),
                                                           :close      => Set["]"]))

  S.def ns(:array), re_conform_val(S.alt(:empty     => ns(:empty_array),
                                         :non_empty => ns(:non_empty_array)))

  S.def ns(:value_list), re_conform_val(S.alt(:val  => conform_key(:val, ns(:json)),
                                              :rest => conforming(S.cat(:val   => conform_key(:val, ns(:json)),
                                                                        :comma => Set[","],
                                                                        :space => ns(:spaces),
                                                                        :tail  => ns(:value_list))) { |m| Array(m[:val]) + Array(m[:tail]) }))

  S.def ns(:empty_object), conforming(S.cat(:open   => Set["{"],
                                            :spaces => ns(:spaces),
                                            :close  => Set["}"])) { Hash[] }

  S.def ns(:non_empty_object), conform_key(:contents, S.cat(:open        => Set["{"],
                                                            :spaces      => ns(:spaces),
                                                            :contents    => ns(:object_contents),
                                                            :more_spaces => ns(:spaces),
                                                            :close       => Set["}"]))

  S.def ns(:object), re_conform_val(S.alt(:empty_object     => ns(:empty_object),
                                          :non_empty_object => ns(:non_empty_object)))

  S.def ns(:kv), conforming(S.cat(:before => ns(:spaces),
                                  :key    => ns(:string),
                                  :colon  => Set[":"],
                                  :after  => ns(:spaces),
                                  :val    => conform_key(:val, ns(:json)))) { |m| Hash[m[:key], m[:val]] }

  S.def ns(:object_contents), re_conform_val(S.alt(:kv   => ns(:kv),
                                                   :rest => conforming(S.cat(:kv    => ns(:kv),
                                                                             :comma => Set[","],
                                                                             :space => ns(:spaces),
                                                                             :tail  => ns(:object_contents))) { |m| m[:kv].merge(m[:tail]) }))

  S.def ns(:json), S.cat(:pre => ns(:spaces),
                         :val => re_conform_val(S.alt(:null    => ns(:null),
                                                      :boolean => ns(:boolean),
                                                      :number  => ns(:number),
                                                      :string  => ns(:string),
                                                      :array   => ns(:array),
                                                      :object  => ns(:object))),
                         :post => ns(:spaces))

  def self.parse(s)
    chars = s.split("")
    result = S.conform(ns(:json), chars)

    if S.invalid?(result)
      raise "noop"
    else
      val = result[:val]
      val
    end
  end
end

class TestJSONParser < Minitest::Test
  def test_keyword_parsing
    assert_parses true,  "true"
    assert_parses false, "false"
    assert_parses nil, "null"
  end

  def test_number_parsing
    assert_parses 42,     "42"
    assert_parses -13,    "-13"
    assert_parses 3.1415, "3.1415"
    assert_parses -0.01,  "-0.01"

    assert_parses 0.2e1,  "0.2e1"
    assert_parses 0.2e+1, "0.2e+1"
    assert_parses 0.2e-1, "0.2e-1"
    assert_parses 0.2E1,  "0.2e1"
  end

  def test_string_parsing
    assert_parses String.new, %Q{""}
    assert_parses "JSON",     %Q{"JSON"}

    assert_parses %Q{nested "quotes"}, '"nested \"quotes\""'
    assert_parses "\n",                %Q{"\\n"}

    skip "doesn't work yet"
    assert_parses "a", %Q{"\\u#{"%04X" % ?a}"}
  end

  def test_array_parsing
    assert_parses [], %Q{[]}

    assert_parses ["foo", "bar", "baz"],  %Q{["foo", "bar", "baz"]}
    assert_parses ["JSON", 3.1415, true], %Q{["JSON", 3.1415, true]}

    skip "incredibly slow"
    assert_parses [1, [2, [3]]],          %Q{[1, [2, [3]]]}
  end

  def test_object_parsing
    assert_parses Hash[], %Q{{}}
    assert_parses Hash["foo" => "bar"], %Q{{"foo": "bar"}}
    assert_parses Hash["foo" => "bar", "baz" => "qux"], %Q{{"foo": "bar", "baz": "qux"}}
    assert_parses Hash["JSON" => 3.1415, "data" => true], %Q{{"JSON": 3.1415, "data": true}}

    skip "incredibly slow"
    assert_parses Hash["Array" => [1, 2, 3], "Object" => {"nested" => "objects"}], <<-END_OBJECT
      {"Array": [1, 2, 3], "Object": {"nested": "objects"}}
    END_OBJECT
  end

  def test_parse_errors
    assert_invalid "{"
    assert_invalid %q{{"key": true false}}

    assert_invalid "["
    assert_invalid "[1,,2]"

    assert_invalid %Q{"}
    assert_invalid %Q{"\\i"}

    assert_invalid "$1,000"
    assert_invalid "1_000"
    assert_invalid "1K"

    assert_invalid "unknown"
  end

  def assert_invalid(json_string)
    assert_raises(RuntimeError) { JSONParser.parse(json_string) }
  end

  def assert_parses(expected_val, json_string)
    if expected_val.nil?
      assert_nil(JSONParser.parse(json_string))
    else
      assert_equal(expected_val, JSONParser.parse(json_string))
    end
  end
end

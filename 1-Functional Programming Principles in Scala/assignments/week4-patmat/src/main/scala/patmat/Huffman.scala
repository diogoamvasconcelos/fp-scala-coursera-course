package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
    abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
    def weight(tree: CodeTree): Int = tree match
    {
        case Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) => weight
        case Leaf(char: Char, weight: Int) => weight
    }
  
    def chars(tree: CodeTree): List[Char] = tree match
    {
        case Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) => chars
        case Leaf(char: Char, weight: Int) => List(char)
    }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
    def times(chars: List[Char]): List[(Char, Int)] =
    {
        def count_char(char: Char, times_list:  List[(Char, Int)]) : List[(Char, Int)] =
        {
            if (times_list == Nil) (char, 1) :: times_list
            else if (times_list.head._1 == char)  (char, times_list.head._2 + 1) :: times_list.tail
            else times_list.head :: count_char(char, times_list.tail)
        }

        def iter_chars(chars: List[Char], times_list:  List[(Char, Int)]): List[(Char, Int)] =
        {
            if (chars == Nil) times_list
            else iter_chars(chars.tail, count_char(chars.head, times_list))
        }

        iter_chars(chars, Nil)
    }
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */

  /*
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  {
      def insert(freq: (Char, Int), sorted_leaves: List[Leaf]): List[Leaf] =
      {
            if (sorted_leaves == Nil) new Leaf(freq._1, freq._2) :: sorted_leaves
            else if (freq._2 < sorted_leaves.head.weight) new Leaf(freq._1, freq._2) :: sorted_leaves
            else sorted_leaves.head :: insert(freq, sorted_leaves.tail)
      }

      def sort(freqs: List[(Char, Int)], sorted_leaves: List[Leaf]): List[Leaf] =
      {

          if (freqs == Nil) sorted_leaves
          else
          {
              insert(freqs.head, sort(freqs.tail, sorted_leaves))
              //sort(freqs.tail, insert(freqs.head, sorted_leaves)) //also works
          }
      }

      sort(freqs, Nil)
  }
  */

    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = //solution using pattern matching
    {
        def insert(freq: (Char, Int), sorted_leaves: List[Leaf]): List[Leaf] = sorted_leaves match
        {
            case List() => List(new Leaf(freq._1, freq._2))
            case x :: xs => if (freq._2 < x.weight)  new Leaf(freq._1, freq._2) :: sorted_leaves else x :: insert(freq, xs)
        }

        def sort(freqs: List[(Char, Int)], sorted_leaves: List[Leaf]): List[Leaf] = freqs match
        {
            case List() => List()
            case x :: xs => insert(x, sort(xs, sorted_leaves))
        }

        sort(freqs, Nil)
    }
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
    def singleton(trees: List[CodeTree]): Boolean = trees match
    {
        case x :: List() => true
        case _ => false //catch all...empty and bigger
    }
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
    def combine(trees: List[CodeTree]): List[CodeTree] = trees match
    {
        case List() => trees
        case x :: List() => trees
        case Leaf(x_char: Char, x_weight: Int) :: xs => xs match
            {
            case Leaf(y_char: Char, y_weight: Int) :: ys => new Fork(trees.head, xs.head, string2Chars(x_char.toString + y_char.toString), x_weight + y_weight) :: xs.tail
            case Fork(y_left: CodeTree, y_right: CodeTree, y_chars: List[Char], y_weight: Int) :: ys => new Fork(trees.head, xs.head, x_char :: y_chars, x_weight + y_weight) :: xs.tail
            }
        case Fork(x_left: CodeTree, x_right: CodeTree, x_chars: List[Char], x_weight: Int) :: xs => xs match
        {
            case Leaf(y_char: Char, y_weight: Int) :: ys => new Fork(trees.head, xs.head, string2Chars(x_chars.toString + y_char.toString), x_weight + y_weight) :: xs.tail
            case Fork(y_left: CodeTree, y_right: CodeTree, y_chars: List[Char], y_weight: Int) :: ys => new Fork(trees.head, xs.head, string2Chars(x_chars.toString + y_chars.toString), x_weight + y_weight) :: xs.tail
        }
    }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
    def until(is_singleton: (List[CodeTree]) => Boolean, combine_func: (List[CodeTree]) => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
  {
      if (is_singleton(trees)) trees
      else until(is_singleton, combine_func)(combine_func(trees))
  }
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
    def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
  {
      def find_chars(bits: List[Bit], curr_tree: CodeTree, message: List[Char]): List[Char] = bits match
      {
          case List() =>  curr_tree match
          {
              case List() => throw new Error("Char not found")
              case Leaf(y_char: Char, _) => y_char :: message
              case Fork(y_left: CodeTree, y_right: CodeTree, _, _) => throw new Error("ERROR")
          }
          case x :: xs => curr_tree match
          {
              case List() => throw new Error("Char not found")
              case Leaf(y_char: Char, _) => y_char :: find_chars(bits, tree, message)
              case Fork(y_left: CodeTree, y_right: CodeTree, _, _) => x match
              {
                  case 0 => find_chars(xs, y_left, message)
                  case 1 => find_chars(xs, y_right, message)
              }
          }
      }

      find_chars(bits, tree, List())
  }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
    def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
  {
      def text_contains(text: List[Char], char: Char): Boolean = text match
      {
          case List() => false
          case x :: xs => if (x == char) true else text_contains(xs, char)
      }

      def encode_chars(text: List[Char], curr_tree: CodeTree, acc: List[Bit]): List[Bit] = text match
      {
          case List() => acc
          case x :: xs => curr_tree match
          {
              case List() => throw new Error("Char not found")
              //case Leaf(y_char: Char, y_weight: Int) => encode_chars(xs, tree, acc)
              case Fork(y_left: CodeTree, y_right: CodeTree, y_chars: List[Char], y_weight: Int) => (y_left, y_right) match
              {
                  case (Leaf(l_char: Char, _), Leaf(r_char: Char, _)) =>
                        if (x == l_char) 0 :: encode_chars(xs, tree, acc)
                        else if (x == r_char) 1 :: encode_chars(xs, tree, acc)
                        else throw new Error("Char not found 1")
                  case (Fork(_, _, l_chars: List[Char], _), Fork(_, _, r_chars: List[Char], _)) =>
                        if (text_contains(l_chars, x)) 0 :: encode_chars(text, y_left, acc)
                        else if (text_contains(r_chars, x)) 1 :: encode_chars(text, y_right, acc)
                        else throw new Error("Char not found 2")
                  case (Leaf(l_char: Char, _), Fork(_, _, r_chars: List[Char], _)) =>
                        if (x == l_char) 0 :: encode_chars(xs, tree, acc)
                        else if (text_contains(r_chars, x)) 1 :: encode_chars(text, y_right, acc)
                        else throw new Error("Char not found 3")
                  case (Fork(_, _, l_chars: List[Char], _), Leaf(r_char: Char, _)) =>
                        if (text_contains(l_chars, x)) 0 :: encode_chars(text, y_left, acc)
                        else if (x == r_char) 1 :: encode_chars(xs, tree, acc)
                        else throw new Error("Char not found 4")
              }
          }
      }

      encode_chars(text, tree, List())
  }
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
    def codeBits(table: CodeTable)(char: Char): List[Bit] = table match
    {
        case List() => throw new Error("Codebit not found in CodeTable")
        case x :: xs => if (x._1 == char) x._2 else codeBits(xs)(char)
    }
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
    def convert(tree: CodeTree): CodeTable =
    {
        def walk_tree(tree: CodeTree, bits: List[Bit], code_table: CodeTable): CodeTable = tree match
        {
            case Leaf(y_char: Char, _) => (y_char, bits) :: code_table
            case Fork(y_left: CodeTree, y_right: CodeTree, _, _) =>
            {
                mergeCodeTables(walk_tree(y_left, bits ::: List(0), code_table), walk_tree(y_right, bits ::: List(1), code_table))
            }
        }

        walk_tree(tree, List(), List())
    }
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable =
    {
        def contains(char: Char, table: CodeTable): Boolean = table match
        {
            case List() => false
            case x :: xs => if (x._1 == char) true else contains(char, xs)
        }

        def merge(table: CodeTable, res: CodeTable): CodeTable = table match
        {
            case List() => res
            case x :: xs => if (contains(x._1, b)) merge(xs, res) else (x._1, x._2) :: merge(xs, res)
        }

        merge(a, b)
    }
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =
    {
        def iter(table: CodeTable, text: List[Char], encoding: List[Bit]): List[Bit] = text match
        {
            case List() => encoding
            case x :: xs => codeBits(table)(x) ::: iter(table, xs, encoding)
        }

       iter(convert(tree), text, List())
    }
  }

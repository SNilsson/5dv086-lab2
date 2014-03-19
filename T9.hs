-- filename	T9.hs
-- authors 	c09mpu@cs.umu.se, c09snn@cs.umu.se
-- date		2014-03-06

module T9 
(get_t9_sequence) where

import Dictionary
import Messages
import Sequences

import Data.Ord
import Data.List
import Data.Char

-- Retrieves the tuple with number and sequence of characters
-- that match the given character.
-- Given a 'C', this function will return (2, "ABC")
get_sequence_from_char :: Char -> (Integer, [Char])
get_sequence_from_char c = head [ t | t <- sequences, elem c (snd t) ]

-- Retrieves a list of tuples with number and sequence of characters
-- given a string of characters.
-- Given the string "WE", this function will return [(9, "WXYZ"), (3, "DEF")]
get_sequence_from_string :: [Char] -> [(Integer, [Char])]
get_sequence_from_string [] = []
get_sequence_from_string str = map get_sequence_from_char str

-- Retrieves all words that match a sequence of key presses.
-- Given the sequence [(1, "ABC"), (2, "DEF")], this function will return
-- [("BE", 4), ("BEEN", 1), ("BETTER", 1) ... ]
get_words_from_sequence :: [(Integer, [Char])] -> [([Char], Integer)] ->  [([Char], Integer)]
get_words_from_sequence [] 	dict = dict
get_words_from_sequence wordseq dict = local_check wordseq dict 0 where
	local_check [] 		dict _ 		= dict
	local_check (h:lt) 	dict pos 	= local_check lt [ t | t <- dict, c <- snd h, pos < length (fst t), fst t !! pos == c ] (pos+1)

-- Retrieves the position of the given string in the list of words
-- that match the given sequence of key presses.
-- Given "BETTER" and the sequence [(1, "ABC"), (2, "DEF")], 
-- which renders the list of words that matches the sequence
-- [("BE", 4), ("BEEN", 1), ("BETTER", 1) ... ],
-- this function returns 1
get_position_for_word :: [Char] -> [(Integer, [Char])] -> [([Char], Integer)] -> Int
get_position_for_word str seq dict = head [ n | n <- [0..length list], fst (list !! n) == str ] where 
	list = sortBy sort_words (get_words_from_sequence seq dict)

-- Calculates the shortest sequence of key presses that are required
-- to render the given word. Find out more about this method in the 
-- report.
get_shortest_sequence :: [Char] -> [([Char], Integer)] -> [(Integer, [Char])]
get_shortest_sequence string	dict	= local_check string (get_sequence_from_string string) (get_sequence_from_string string) dict where
	local_check _		seq	[]	_	= seq
	local_check string	seq1	seq2 	dict 	= 
		if (get_position_for_word string seq1 dict) + length seq1 < (get_position_for_word string seq2 dict) + length seq2
			then local_check string seq1 (init seq2) dict
			else local_check string seq2 (init seq1) dict

-- Converts a sequence of key presses into a string of numbers.
-- Given [(9, "WXYZ"), (3, "DEF")], the function renders "93"
get_numbers_from_sequence :: [(Integer, [Char])] -> [Char]
get_numbers_from_sequence [] 	= []
get_numbers_from_sequence (h:lt)	= show (fst h) ++ get_numbers_from_sequence lt

-- Retrieve the shortest T9-sequence for the given string 
-- (it cannot contain any spaces)
-- Given "THOSE", the function returns 846^
get_t9_for_word :: [Char] -> [Char]
get_t9_for_word string = get_numbers_from_sequence (get_shortest_sequence string dictionary) ++ concat (replicate (get_position_for_word string (get_shortest_sequence string dictionary) dictionary) "^")

-- Retrieve the shortest T9-sequence for the given list of strings
-- Given ["THOSE", "WHO", "DO", "NOT", ... ], the function returns 846^094^0306 ... 
get_t9_for_word_list :: [[Char]] -> [Char]
get_t9_for_word_list [] 	= []
get_t9_for_word_list (h:lt) 	| length lt == 0 	= get_t9_for_word h
				| length lt > 0		= get_t9_for_word h ++ get_numbers_from_sequence ([get_sequence_from_char ' ']) ++ get_t9_for_word_list lt

-- Retrieve the shortest T9-sequence for an entire sentence
-- Given "THOSE WHO DO NOT HAVE GOALS ARE DOOMED TO WORK FOR THOSE WHO DO"
-- the function returns "846^094^030604^04602^036608096703^0846^094^03"
get_t9_sequence :: [Char] -> [Char]
get_t9_sequence str = get_t9_for_word_list (words str)

-- Sorts two words on the form ([Char], Integer)
-- by priority (Integer), then the word ([Char])
sort_words :: ([Char], Integer) -> ([Char], Integer) -> Ordering
sort_words (as, ai) (bs, bi)
	| ai == bi = compare as bs
	| ai > bi = LT
	| ai < bi = GT


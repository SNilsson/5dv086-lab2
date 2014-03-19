import T9
import Messages

a = [ str ++ "\n" ++ get_t9_sequence str ++ "\n\n" | str <- messages ]

--Prints the output made to 'a' and writes it to 'test.txt'.
-- writeFile "test.txt" $ unwords a

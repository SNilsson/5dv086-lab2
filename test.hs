import T9
import Messages

a = [ str ++ "\n" ++ get_t9_sequence str ++ "\n\n" | str <- messages ]
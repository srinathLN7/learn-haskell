# Caesar-Cipher

A well known encoding method is the Caesar cipher, named after its use by Julius Caesar more than 2000 years ago.
To encode a string, Caesar simply replaced each letter in the string by the letter three places further down in the alphabet, wrapping at the end of the alphabet.

For example, `"Hakell is fun"` would be encoded as `"Kdvnhoo lv ixq"` 

The algorithm written above decodes the given string encoded using the caesar-cipher method by analyzing the frequency of occurence of each alphabets (both lower case and upper case) in the string and comparing it to the standard table which contains the frequency of occurence of all lower case letters over a large volume of texts. The objective is to minimize the difference of squares between the two tables.     

# advent2021

## problem 14

- problem input growing too fast for B (overuse of memory of List())
- array growing over index range (2^32-1)
- counting pairs overflow the pairs and the result
- sequence could have the same letter in the begginning and in the end


## problem 15

- priority queue should keep the entries with the same priority
- generation of the large input should use i%9

## problem 16

- misunderstood how length field should be evaluated
- missed the part where the input might contain irrelevant data (trailing zeroes)
- overflow got me again =(
- multi-packs int parsing failed due to error in shifting 

## problem 17

- partb should include negative velocities for Y
- not many mistakes today, as I brute-forced my way 

## problem 18

- tried to use Either[Pair, Int] instead of trait Node / case class INode / case class Leaft and ended up messing up
- did not pay enough attention to the operation being non comunitative

## problem 19

## problem 20

- image is infinite but the pixels out of bounds can 'change' after enhancing (need to keep track of what they are)

## problem 21

- misread the problem (sent the low score and not the high score)
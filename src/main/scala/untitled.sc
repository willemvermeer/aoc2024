0.toString.toCharArray.head

"1234".substring(0, 2).toInt

def sum(count: Int): Int = {
  if (count == 10) 1
  else 1 + sum(count + 1)
}
sum(0)
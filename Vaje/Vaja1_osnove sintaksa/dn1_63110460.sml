fun naslednje (n:int):int =
	n+1
	
fun max (a:int, b:int):int =
	if a > b
	then a
	else b
	
fun fakulteta (n:int):int =
	if n = 0
	then 1
	else n*fakulteta(n-1)
	
fun fib (n:int):int =
	if n = 1 orelse n = 2
	then 1
	else fib(n-1) + fib(n-2)
	
fun pow (n:int, m:int):int =
	if m = 0
	then 1
	else n * pow(n, m-1)
	
fun niDeliteljevNad(n:int, d:int):bool =
	if n mod d = 0
	then false
	else pow(d,2) >= n orelse niDeliteljevNad(n, naslednje(d))
	
fun jePrastevilo (n:int):bool =
	if n = 1
	then false
	else if n = 2
		then true
		else niDeliteljevNad(n, 2)

fun binarno (n:int):string =
	if n = 0
	then "0"
	else if n = 1
		then "1"
		else binarno(n div 2) ^ binarno(n mod 2)
		
fun gcd (x:int, y:int):int =
	if y = 0
	then x
	else gcd(y, x mod y);

gcd(6,9);


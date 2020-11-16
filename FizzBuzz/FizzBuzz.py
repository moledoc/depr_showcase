
fizz = lambda x : "Fizz" if (x%3==0) else ""
buzz = lambda x : "Buzz" if (x%5==0) else ""

def fizzbuzz(x):
    isFizzBuzz = fizz(x) + buzz(x)
    if isFizzBuzz!="":
        print(isFizzBuzz)
    else:
        print(x)

for i in range(1,101):
    fizzbuzz(i)

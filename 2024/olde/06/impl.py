from math import log10, sqrt
from functools import reduce

def read_input(filename:str):
    f = open(filename)
    times_str = f.readline()
    dists_str = f.readline()
    times = [int(x) for x in times_str.split(' ')[1:] if x != '']
    dists = [int(x) for x in dists_str.split(' ')[1:] if x != '']
    return (times, dists)

def read_input(filename:str):
    f = open(filename)
    times_str = f.readline()
    dists_str = f.readline()
    times = [int(x) for x in times_str.split(' ')[1:] if x != '']
    dists = [int(x) for x in dists_str.split(' ')[1:] if x != '']
    return (times, dists)

def read_input_2(filename:str):
    f = open(filename)
    times_str = f.readline()
    dists_str = f.readline()
    time_str = reduce(lambda x,y:x+y, [x for x in times_str.split(' ')][1:])
    dist_str = reduce(lambda x,y:x+y, [x for x in dists_str.split(' ')][1:])
    return int(time_str), int(dist_str)
    
def exact_int(a) -> bool:
    return a == int(a)

def ways_to_win(t, d):
    delta = sqrt(t**2 - 4*d)
    min = ((t - delta) / 2)
    max = ((t + delta)/2)

    int_max = int(max) if not exact_int(max) else int(max) - 1
    int_min = int(min) + 1
    
    return int_max - int_min + 1

def shitty_ways_to_win(t:int, d:int) -> int:
    res:int = 0
    for i in range(1,t):
        if i * (t - i) > d:
            res+=1

    return res

def cat_int(a, b):
    return a * 10**log10(b) + b

if __name__=="__main__":
    
    times, dists = read_input("real")
    margins = [ways_to_win(t, d) for (t, d) in zip(times, dists)]
    shitty = [shitty_ways_to_win(t, d) for (t, d) in zip(times, dists)]
    print(times)
    print(dists)
    print(margins)
    print(shitty)

    print("result : ")
    print(reduce(lambda x,y: x*y, shitty))

    big_time, big_dist = read_input_2("real")

    print(big_time)
    print(big_dist)

    print(ways_to_win(big_time, big_dist))
    

before = {} # numbers that must be before
after = {} # numbers that must be after

def check_precedence(lst):
    for i in range(len(lst)):
        for j in range(i, len(lst)):
            if lst[i] in before.keys() and (lst[j] in before[lst[i]]):
                return False
        # for j in range(0, i):
        #     if lst[i] in after.keys() and (lst[j] in after[lst[i]]):
        #         return False
    return True

def sort(lst, test):
    for i in range(len(lst)):
        for j in range(len(lst)-1):
            if test(lst[j], lst[j+1]):
                tmp = lst[j]
                lst[j] = lst[j+1]
                lst[j+1] = tmp

    return lst

def main(filename):
    total = 0
    with open(filename) as f:
        done_precedence=False
        for line in f:
            if len(line.strip()) == 0:
                done_precedence = True
            elif done_precedence:
                row = [int(x) for x in line.split(',')]
                if not check_precedence(row):
                    sort(row, lambda x, y: x in before[y])
                    total += row[len(row)//2]
            else:
                numbers = [int(x) for x in line.split('|')]
                first = numbers[0]
                last = numbers[1]
                if first not in after.keys():
                    after[first] = []
                if last not in before.keys():
                    before[last] = []
                before[last].append(first)
                after[first].append(last)


    print(total)

# if __name__=='__main__':

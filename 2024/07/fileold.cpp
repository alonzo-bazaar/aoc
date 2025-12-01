#include <cstdint>
#include<iostream>

#include <fstream>
#include <string>
#include <vector>

std::pair<long, std::vector<long>> parse_line(const std::string& s) {
    size_t start = 0;
    size_t end = s.find(':');
    long left = stol(s.substr(start, end));

    start = end + 2; // skip ": "
    end = s.find(' ', start);

    std::vector<long> right {};
    while(end != std::string::npos) {
        right.push_back(stol(s.substr(start, end)));
        start = end + 1;
        end = s.find(' ', start);
    }
    right.push_back(stol(s.substr(start, end)));

    return std::pair {left, right};
}

bool can_obtain(long left, const std::vector<long>& right) {
    for(uint64_t sgargiullo = (1 << right.size()) - 1; sgargiullo != 0; --sgargiullo) {
        long candidate = right[0];
        size_t i = 1; uint64_t mask = 2;
        for(; i<right.size(); mask = mask<<1, ++i) {
            if(sgargiullo & mask) {
                candidate += right[i];
            }
            else {
                candidate *= right[i];
            }
        }
        if (candidate == left) {
            return true;
        }
    }
    return false;
}

int main() {
    std::ifstream f("input");
    std::string s;

    long long total = 0;
    while(std::getline(f, s)) {
        auto p = parse_line(s);
        if(can_obtain(p.first, p.second)) {
            total += p.first;
            std::cout<<"line \""<<s<<"\" is good"<<std::endl;
        }
    }
    std::cout<<"and the total is : "<<total<<std::endl;
}

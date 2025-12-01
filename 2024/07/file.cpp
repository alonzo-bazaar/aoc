#include <cmath>
#include <cstdint>
#include<iostream>

#include <fstream>
#include <string>
#include <vector>

std::pair<unsigned long long, std::vector<unsigned long long>>
    parse_line(const std::string& s) {
    auto start = 0;
    auto end = s.find(':');
    auto left = stol(s.substr(start, end));

    start = end + 2; // skip ": "
    end = s.find(' ', start);

    std::vector<unsigned long long> right {};
    while(end != std::string::npos) {
        right.push_back(stol(s.substr(start, end)));
        start = end + 1;
        end = s.find(' ', start);
    }
    right.push_back(stol(s.substr(start, end)));

    return std::pair {left, right};
}

bool can_obtain(unsigned long long left,
                const std::vector<unsigned long long>& right) {
    for(auto sgargiullo = (int64_t)pow(3, right.size()-1) - 1;
        sgargiullo >= 0; --sgargiullo) {
        auto candidate = right[0];
        auto copy = sgargiullo;
        for(auto i = 1; i<right.size(); ++i) {
            switch(copy%3) {
            case 0:
                candidate += right[i];
                break;
            case 1:
                candidate *= right[i];
                break;
            case 2:
                // https://stackoverflow.com/questions/1489830/efficient-way-to-determine-number-of-digits-in-an-integer
                auto tmp = right[i];
                auto digits = 0; do { tmp /= 10; digits++; } while (tmp != 0);
                auto powten = 10; while(--digits != 0) { powten*=10; }

                candidate *= powten;
                candidate += right[i];
                break;
            }
            copy/=3;
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

    unsigned long long total = 0;
    while(std::getline(f, s)) {
        auto p = parse_line(s);
        if(can_obtain(p.first, p.second)) {
            if((total + p.first) <= total)
                std::cout<<"HELLO BOIS OVERFLOW SHOW"<<std::endl;

            total += p.first;
            std::cout<<"line \""<<s<<"\" is good"<<std::endl;
        }
    }
    std::cout<<"and the total is : "<<total<<std::endl;
}

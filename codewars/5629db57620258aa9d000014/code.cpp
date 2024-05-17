class Frequencies {
public:
    Frequencies() : data() {}

    int max() const { return std::max(data[0], data[1]); }
    std::string prefix() const
    {
        if (data[0] == data[1]) return "=:";
        if (max() == data[0]) return "1:";
        return "2:";
    }

    int data[2];
};


class Mix {
public:
  static bool substring_less(const std::string &s1, const std::string &s2) {
    if (s1.size() == s2.size()) {
      return s1 < s2;
    } else {
      return s1.size() > s2.size();
    }
  }

  static std::string join(std::vector<std::string> elts, std::string separator) {
    std::ostringstream oss;
    std::copy(elts.begin(), elts.end() - 1, std::ostream_iterator<std::string>(oss, separator.c_str()));
    oss << elts.back();
    return oss.str();
  }

  static std::string mix(const std::string &s1, const std::string &s2) {
    Frequencies freq[26];
    for (char ch : s1)
      if (islower(ch))
        ++freq[ch-'a'].data[0];
    for (char ch : s2)
      if (islower(ch))
        ++freq[ch-'a'].data[1];

    std::vector<std::string> segs;
    for (int i = 0; i < 26; i++) {
      if (freq[i].max() <= 1) continue;
      auto maxFreq = freq[i].max();
      std::string seg = freq[i].prefix();
      for (int j = 0; j < maxFreq; j++)
        seg += char('a' + i);
      segs.push_back(seg);
    }
    std::sort(segs.begin(), segs.end(), substring_less);
    return join(segs, "/");
  }
};

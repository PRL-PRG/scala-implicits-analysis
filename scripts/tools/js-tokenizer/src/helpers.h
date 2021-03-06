#pragma once

#include <sys/types.h>
#include <sys/stat.h>

#include <ctime>
#include <cmath>

#include <sstream>
#include <iostream>
#include <iomanip>

#include "hashes/md5.h"

#include <cassert>


/** Shorthand for converting different types to string as long as they support the std::ostream << operator.
*/
#define STR(WHAT) static_cast<std::stringstream&>(std::stringstream() << WHAT).str()

inline unsigned char fromHex(char what) {
    if (what >= 'a')
        return (what - 'a') + 10;
    else
        return what - '0';
}


/** Compressed hash so that it occupies less space.
 */
struct Hash {
    uint64_t first;
    uint64_t second;

    Hash():
        first(0),
        second(0) {
    }

    static Hash Parse(char const * from) {
        Hash h;
        unsigned char * x = reinterpret_cast<unsigned char *>(&h);
        unsigned j = 0;
        for (unsigned i = 0; i < 16; ++i) {
            x[i] = fromHex(from[j]) * 16 + fromHex(from[j + 1]);
            j += 2;
        }
        return h;
    }

    /** Constructs the hash from given string as computed by the hashlibrary.
     */
    Hash(MD5 & from) {
        from.getHash(reinterpret_cast<unsigned char *>(this));
    }

    Hash(std::string const & from) {
        MD5 md5;
        md5.add(from.c_str(), from.size());
        md5.getHash(reinterpret_cast<unsigned char *>(this));
    }

    Hash(Hash const & from):
        first(from.first),
        second(from.second) {
    }

    Hash & operator = (Hash const & other) {
        first = other.first;
        second = other.second;
        return *this;
    }

    /** Returns a packed representation of the hash that occupies only 16bytes utilizing the entire char range.

      Note this is only to make sure the token hashes occupy the smallest memory in tokens map, it should never leak to databases or output as it will generate super silly special characters.
*/
    std::string pack() const {
        std::stringstream s;
        unsigned char const * x = reinterpret_cast<unsigned char const *>(this);
        for (int i = 0; i < 16; ++i)
            s << (char) (x[i]);
        return s.str();
    }

    bool operator == (Hash const & other) const {
        return first == other.first and second == other.second;
    }

    bool operator != (Hash const & other) const {
        return first != other.first or second != other.second;
    }

    friend std::ostream & operator << (std::ostream & s, Hash const & h) {
        static const char dec2hex[16+1] = "0123456789abcdef";
        unsigned char const * x = reinterpret_cast<unsigned char const *>(&h);
        for (int i = 0; i < 16; ++i) {
            s << dec2hex[(x[i] >> 4) & 15];
            s << dec2hex[(x[i]) & 15];
        }
        return s;
    }
};

namespace std {

    template<>
    struct hash<::Hash> {

        std::size_t operator()(::Hash const & h) const {
            return std::hash<uint64_t>{}(h.first) + std::hash<uint64_t>{}(h.second);
        }

    };
}

inline double round(double what, unsigned precission) {
    double x = pow(10, precission);
    return round(what * x) / x;
}

inline std::string pct(unsigned x, unsigned total) {
    if (total == 0)
        return "";
    double pct = (x * 100.0) / total;
    if (pct > 10)
        return STR(round(pct, 0));
    else
        return STR(round(pct, 1));
}

inline std::string xbytes(unsigned long b) {
    double x = b;
    if ( x < 1024)
        return STR(x << " b");
    x /= 1024;
    if (x < 1024)
        return STR(round(x, 2)  << " Kb");
    x /= 1024;
    if (x < 1024)
        return STR(round(x, 2)  << " Mb");
    x /= 1024;
    if (x < 1024)
        return STR(round(x, 2)  << " Gb");
    x /= 1024;
    return STR(round(x, 2)  << " Tb");
}

inline std::string escape(Hash const & what) {
    return STR("\"" << what << "\"");
}


inline std::string escape(std::string const & from) {
    std::string result;
    result.reserve(from.size());
    result += "\"";
    for (char c : from) {
        switch (c) {
            case 0:
                result += "\\0";
                break;
            case '\'':
            case '\"':
            case '\\':
            case '_':
            case '%':
                result += "\\";
                result += c;
                break;
            case '\b':
                result += "\\b";
                break;
            case '\n':
                result += "\\n";
                break;
            case '\r':
                result += "\\r";
                break;
            case '\t':
                result += "\\t";
                break;
            case 26:
                result += "\\Z";
                break;
            default:
                result += c;
        }
    }
    result += "\"";
    return result;
}

inline unsigned timestampFrom(std::string const & col) {
    tm tm;
    strptime(col.c_str(), "%Y-%m-%d %H:%M:%S", &tm);
    return mktime(&tm);
}

inline bool isDirectory(std::string const & path) {
    char *fullpath = realpath(path.c_str(), NULL);
    struct stat s;
    bool is_dir = false;

    if (lstat(fullpath,&s) == 0)
        is_dir = S_ISDIR(s.st_mode);

    free(fullpath);

    return is_dir;
}

inline bool isFile(std::string const & path) {
    struct stat s;
    if (lstat(path.c_str(),&s) == 0)
        return S_ISREG(s.st_mode);
    return false;
}

inline void createDirectory(std::string const & path) {
    if (system(STR("mkdir -p " << path).c_str()) != EXIT_SUCCESS)
        throw STR("Unable to create directory " << path);
}

inline void createDirectoryForFile(std::string const & filename) {
    assert(filename.rfind("/") != std::string::npos);
    std::string dir = filename.substr(0, filename.rfind("/"));
    if (system(STR("mkdir -p " << dir).c_str()) != EXIT_SUCCESS)
        throw STR("Unable to create directory " << dir << " for file " << filename);
}

inline std::string exec(std::string const & what, std::string const & path ) {
    char buffer[128];
    std::string result = "";
    std::string cmd = STR("cd \"" << path << "\" && " << what << " 2>&1");
    //std::cout << cmd << std::endl;
    std::shared_ptr<FILE> pipe(popen(cmd.c_str(), "r"), pclose);
    if (not pipe)
        throw STR("Unable to execute command " << cmd);
    while (not feof(pipe.get())) {
        if (fgets(buffer, 128, pipe.get()) != nullptr)
            result += buffer;
    }
    return result;
}

inline std::string time(double secs) {
    unsigned seconds = (unsigned) secs;

    unsigned d = seconds / (3600 * 24);
    seconds = seconds % (3600 * 24);

    unsigned h = seconds / 3600;
    seconds = seconds % 3600;
    unsigned m = seconds / 60;
    unsigned s = seconds % 60;
    return STR(d << ":" << std::setfill('0') << std::setw(2) << h << ":" << std::setfill('0') << std::setw(2) << m << ":" << std::setfill('0') << std::setw(2) << s);
}







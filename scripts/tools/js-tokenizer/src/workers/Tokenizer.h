#pragma once

#include <set>

#include "../data.h"
#include "../worker.h"
#include "Merger.h"

#include "../tokenizers/generic.h"


class TokenizerJob : public std::shared_ptr<ClonedProject> {
public:
    TokenizerJob() {
    }

    TokenizerJob(std::shared_ptr<ClonedProject> ptr):
        std::shared_ptr<ClonedProject>(ptr) {
    }

    friend std::ostream & operator << (std::ostream & s, TokenizerJob const & j) {
        s << "Project id " << j->id << ", url: " << j->cloneUrl();
        return s;
    }

};

class Tokenizer : public Worker<TokenizerJob> {
public:
    static char const * Name() {
        return "TOKENIZER";
    }

    Tokenizer(unsigned index):
        Worker<TokenizerJob>(Name(), index) {
    }

    static unsigned TotalFiles() {
        return totalFiles_;
    }

    static unsigned long TotalBytes() {
        return totalBytes_;
    }

    static void AddTokenizer(TokenizerKind kind) {
        tokenizers_.insert(kind);
    }

/*    static void FlushBuffers() {
        files_.flush();
        filesExtra_.flush();
    } */

private:
    bool isLanguageFile(std::string const & filename) {
        std::string ext = ".scala";
        return (filename.rfind(ext) == filename.size() - ext.size());
    }

    bool isArchive(std::string const & data) {
        return (data.size() >= 4 and data[0] == 'P' and data[1] =='K' and data[2] == '\003' and data[3] == '\004');
    }

    bool isBinary(std::string const & data) {
        return (data.size() >= 15) and
                data[0] == 0 and
                data[1] == 5 and
                data[2] == 22 and
                data[3] == 7 and
                data[4] == 0 and
                data[5] == 2 and
                data[6] == 0 and
                data[7] == 0; // then we have Mac OS X
    }

    void encodeUTF8(unsigned codepoint, std::string & into) {
        if (codepoint < 0x80) {
            into += static_cast<char>(codepoint);
        } else if (codepoint < 0x800) {
            into += static_cast<char>(0xc0 + (codepoint >> 6));
            into += static_cast<char>(0x80 + (codepoint & 0x3f));
        } else if (codepoint < 0x10000) {
            into += static_cast<char>(0xe0 + (codepoint >> 12));
            into += static_cast<char>(0x80 + ((codepoint >> 6) & 0x3f));
            into += static_cast<char>(0x80 + (codepoint & 0x3f));
        } else if (codepoint < 0x200000) {
            into += static_cast<char>(0xf0 + (codepoint >> 18));
            into += static_cast<char>(0x80 + ((codepoint >> 12) & 0x3f));
            into += static_cast<char>(0x80 + ((codepoint >> 6) & 0x3f));
            into += static_cast<char>(0x80 + (codepoint & 0x3f));
        } else if (codepoint < 0x4000000) {
            into += static_cast<char>(0xf8 + (codepoint >> 24));
            into += static_cast<char>(0x80 + ((codepoint >> 18) & 0x3f));
            into += static_cast<char>(0x80 + ((codepoint >> 12) & 0x3f));
            into += static_cast<char>(0x80 + ((codepoint >> 6) & 0x3f));
            into += static_cast<char>(0x80 + (codepoint & 0x3f));
        } else {
            if (codepoint >= 0x80000000) {
                throw (STR("Unknown unicode character " << codepoint << " in file "));
            }
            into += static_cast<char>(0xfc + (codepoint >> 30));
            into += static_cast<char>(0x80 + ((codepoint >> 24) & 0x3f));
            into += static_cast<char>(0x80 + ((codepoint >> 18) & 0x3f));
            into += static_cast<char>(0x80 + ((codepoint >> 12) & 0x3f));
            into += static_cast<char>(0x80 + ((codepoint >> 6) & 0x3f));
            into += static_cast<char>(0x80 + (codepoint & 0x3f));
        }
    }

    void convertUTF16be(std::string & source) {
        Log("Converting from UTF16be");
        ++utf16be_;
        std::string result;
        result.reserve(source.size());
        size_t i = 2, e = source.size();
        while (i < e) {
            unsigned cp = ((unsigned char)source[i] << 8) + (unsigned char)source[i+1];
            i += 2;
            // this might be 2x codepoint
            if (cp >= 0xd800 and cp < 0xe000) {
                unsigned cp2 = ((unsigned char)source[i] << 8) + (unsigned char)source[i+1];
                if (cp2 >= 0xdc00) {
                    i += 2;
                    cp = 0x10000 + ((cp = 0xd800) << 10) + (cp2 - 0xdc00);
                }
            }
            encodeUTF8(cp, result);
        }
        source = std::move(result);
    }


    void convertUTF16le(std::string & source) {
        Log("Converting from UTF16le");
        ++utf16le_;
        std::string result;
        result.reserve(source.size());
        size_t i = 2, e = source.size();
        while (i < e) {
            unsigned cp = ((unsigned char)source[i + 1] << 8) + (unsigned char)source[i];
            i += 2;
            // this might be 2x codepoint
            if (cp >= 0xd800 and cp < 0xe000) {
                unsigned cp2 = ((unsigned char)source[i + 1] << 8) + (unsigned char)source[i];
                if (cp2 >= 0xdc00) {
                    i += 2;
                    cp = 0x10000 + ((cp = 0xd800) << 10) + (cp2 - 0xdc00);
                }
            }
            encodeUTF8(cp, result);
        }
        source = std::move(result);
    }

    void utf8(std::string & source) {
        if (source.size() >= 2) {
            if ((source[0] == (char) 0xff and source[1] == (char)0xfe)) {
                convertUTF16le(source);
            } else if (source[0] == (char) 0xfe and source[1] == (char)0xff) {
                convertUTF16be(source);
            }
        }
    }

    virtual void process() {
        // open the cdates
        std::ifstream cdates(STR(job_->path << "/cdate.js.tokenizer.txt"));
        if (not cdates.good())
            throw STR("Downloader must make sure that the cdate.js.tokenizer.txt file exists");

        // parse the cdates file and tokenize each language file found

        int date = 0;
        std::set<std::string> visited;
        while (not cdates.eof()) {
            std::string tmp;
            std::getline(cdates, tmp, '\n');
            if (tmp == "") { // if the line is empty, next line will be timestamp
                date = 0;
            } else if (date == 0) { // read timestamp
                date = std::atoi(tmp.c_str());
            } else { // all other lines are actual files
                if (visited.find(tmp) == visited.end()) {
                    visited.insert(tmp);
                    if (isLanguageFile(tmp)) {
                        try {
                            tokenize(tmp, date);
                        } catch (std::string const & e) {
                            throw STR(e << " in file " << tmp);
                        }
                    }
                }
            }
        }
    }

    Hash hash(TokensMap const & tm) {
        MD5 md5;
        for (auto const & i : tm.tokens) {
            md5.add(i.first.c_str(), i.first.size());
            md5.add(& i.second, sizeof(i.second));
        }
        return md5;
    }

    template<typename T>
    bool tokenize(int id, std::string const & relPath, int cdate, std::string & file, unsigned length, Hash fileHash) {
        // create the tokenized file
        std::shared_ptr<TokenizedFile> tf(new TokenizedFile(id, job_, relPath, cdate));
        // create the tokenizer and tokenize the file
        T tokenizer(file, tf);
        tokenizer.tokenize();
        if (tf->errors == 0) {
            std::shared_ptr<TokensMap> tm = tokenizer.tokensMap();
            // fill in additional details
            tf->tokenizer = T::kind;
            tf->bytes = length;
            tf->fileHash = fileHash;
            tf->tokensHash = hash(*tm);
            tf->uniqueTokens = tm->tokens.size();
            // now schedule the merger for the tokens map, which has pointer to the file as well
            Merger::Schedule(MergerJob(tm));
            return true;

        } else {
            return false;
        }
    }

    void tokenize(std::string const & relPath, int cdate) {

        std::string path = STR(job_->path << "/" << relPath);
        if (not isFile(path)) {
            //Log(STR("Skipping " << path << " as it is not a file"));
            return;
        }
        std::ifstream s(path, std::ios::in | std::ios::binary);
        // it is actually not an error if the file is not found, just github report also deleted files
        if (s.good()) {
            //try {
            // load the entire file
            s.seekg(0, std::ios::end);
            unsigned long resizeSize = s.tellg();
            std::string data;
            data.resize(resizeSize);
            s.seekg(0, std::ios::beg);
            s.read(& data[0], data.size());
            s.close();
            if (isArchive(data)) {
                Log(STR(path << " seems to be archive"));
                ++archives_;
            } else if (isBinary(data)) {
                Log(STR(path << " seems to be binary"));
                ++binary_;
            } else {
                unsigned length = data.size();
                Hash fileHash(data);
                // do UTF conversion of required
                utf8(data);
                // get new ID for the file
                int id = TokenizedFile::GetNewId();
                // valid file, let's tokenize
                bool isValid = true;
                tokenize<GenericTokenizer>(id, relPath, cdate, data, length, fileHash);
                // send the file to the backend
                Buffer::Get(Buffer::Kind::Files).append(STR(
                    id << "," <<
                    job_->id << "," <<
                    escape(relPath) << "," <<
                    escape(fileHash)));
                Buffer::Get(Buffer::Kind::FilesExtra).append(STR(
                    id << "," <<
                    cdate));
                // update counters
                ++totalFiles_;
                totalBytes_ += length;
            }
        }
    }

    static std::atomic_uint archives_;
    static std::atomic_uint binary_;
    static std::atomic_uint utf16be_;
    static std::atomic_uint utf16le_;
    static std::atomic_uint totalFiles_;
    static std::atomic_ulong totalBytes_;

    //static Buffer files_;
    //static Buffer filesExtra_;

    static std::set<TokenizerKind> tokenizers_;
};

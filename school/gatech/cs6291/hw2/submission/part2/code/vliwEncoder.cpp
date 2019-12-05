#include <iostream>
#include <stdio.h>
#include <fstream>
#include <sstream>
#include <queue>
#include <cmath>
#include "vliwEncoder.h"

#define MAX_INSTR_WIDTH 100

/*
Read instructions from an input file and return a char array 
*/
std::vector<std::string> readInput(const char* fname)
{
    std::string str;
    std::vector<std::string> instructions;
    
    std::ifstream filestream(fname);
    
    while (std::getline(filestream, str))
    {
        int current_position = 0;
        std::size_t pos = str.find("\tc0    ");
        if (pos == 0)
        {
            instructions.push_back(str);
        } else {
            std::size_t f = str.find(";;");
            if (f == 0)
                instructions.push_back(str);
        }
    }
   
   return instructions;
}

/*
Print an encoded VLIW instruction to a file. The encoded instruction should be a character array and terminated by '\0' character
*/
void printOutput(const char* fname, std::vector<std::string> encodedVLIW)
{
    std::ofstream outfile;
    outfile.open(fname);
  
    for(int i = 0; i < encodedVLIW.size(); i++)
        outfile << encodedVLIW[i] << "\n";

    outfile.close();
}

/*
TODO : Write any helper functions that you may need here. 
*/

const std::string WHITESPACE = " \n\r\t\f\v";

std::string ltrim(const std::string& s) 
{
    size_t start = s.find_first_not_of(WHITESPACE);
    return (start == std::string::npos) ? "" : s.substr(start);
}

std::string rtrim(const std::string& s)
{
    size_t end = s.find_last_not_of(WHITESPACE);
    return (end == std::string::npos) ? "" : s.substr(0, end+1);
}

std::string trim(const std::string& s)
{
    return rtrim(ltrim(s));
}

void print_bits(std::ostream& out, const size_t width, const int mask)
{
    for (int bit = width-1; bit >= 0; bit--) {
        out << ((mask & (1 << bit)) ? '1' : '0');
    }
}

/* removes the cluster prefix and any comments; trims any whitespace */
void print_clean_instr(std::ostream& out, std::string& instr) 
{
    const std::string t = instr.substr(7);
    int pos = t.find("#");
    if (pos > 0) {
        const std::string r = trim(t.substr(0, pos));
        out << "\t" << r;
    } else {
        out << "\t" << trim(t);
    }
}

std::vector<std::string> encode_instructions(std::vector<std::string> instructions, std::string (*encoder)(size_t*, const int, const size_t, std::queue<std::string>&))
{
    std::vector<std::string> encodedVLIW;

    std::queue<std::string> buffer;
    size_t uncompressed_syllables = 0;
    size_t syllables = 0;
    size_t overhead = 0;
    for (std::vector<std::string>::iterator it = instructions.begin(); it != instructions.end(); ++it) {
        const std::string& s = *it;

        if (s.find(";;") == string::npos) {
            uncompressed_syllables++;
            buffer.push(s);
        } else {

            /* once we have a complete instruction, determine the mask and
               remove any NOP instructions */
            size_t instr_size = buffer.size();
            unsigned char mask = 0;
            std::queue<std::string> compressed_instructions;
            for (int instr = 0; !buffer.empty(); instr++) {
                const std::string& i = buffer.front();
                if (i != "\tc0    NOP") {
                    mask |= (1 << (instr_size-instr-1));
                    compressed_instructions.push(i);
                    syllables++;
                }
                buffer.pop();
            }

            /* now that the mask is calculated, print it and the compressed
               instruction */
            size_t mask_size = 0;
            const std::string& line = encoder(&mask_size, mask, instr_size, compressed_instructions);
            overhead += mask_size;
            encodedVLIW.push_back(line);
        }
    }

    size_t uncompressed_size = uncompressed_syllables * 32;
    size_t compressed_size = syllables * 32 + overhead;
    std::cout << "# Uncompressed Syllables: " << uncompressed_syllables << " " << uncompressed_size << " (bits)" 
              << "\t# VEX Syllables: " << syllables  << " " << syllables * 32 << " (bits)"
              << "\tOverhead Size: " << overhead
              << "\tTotal: " << uncompressed_size << " vs " << compressed_size
              << " " << ((float)compressed_size / (float)uncompressed_size) * 100.0 << "%" << std::endl;

    return encodedVLIW;
}


std::string format_masked_VLIW(size_t* overhead, const int mask, const size_t instr_size, std::queue<std::string>& compressed_instructions) 
{
    std::stringstream out;
    out << "c0\t"; /* TODO: hard-coded to c0; this could be more dynamic */

    print_bits(out, instr_size, mask);
    *overhead = (instr_size == 4) ? 4 : 8;

    while (!compressed_instructions.empty()) {
        print_clean_instr(out, compressed_instructions.front());
        compressed_instructions.pop();
    }
    out << std::endl << ";;";
    return out.str();
}


std::string format_template_VLIW(size_t* overhead, const int mask, const size_t instr_size, std::queue<std::string>& compressed_instructions)
{
    std::stringstream out;
    out << "c0\t"; /* TODO: hard-coded to c0; this could be more dynamic */

    int last = 0;
    size_t syllables = 0;
    for (int bit = instr_size-1; bit >= 0; bit--) {
        if (mask & (1 << bit)) {
            int delta = instr_size-1 - bit - last;
            if (bit == instr_size-1) delta += 1;
            print_bits(out, (instr_size == 4) ? 2 : 3, delta);
            last = (instr_size-1-bit);
            syllables++;
        }
    }
    if (syllables == 0) {
        *overhead = (size_t)(std::log(instr_size));
    } else {
        *overhead = (size_t)(syllables * std::log(instr_size));
    }

    while (!compressed_instructions.empty()) {
        print_clean_instr(out, compressed_instructions.front());
        compressed_instructions.pop();
    }
    out << std::endl << ";;";
    return out.str();
}


/*
Input : std::vector<std::string> instructions. The input is a vector of strings. Each string in the vector is one line in the vex code. A line can be a instruction or a ';;'

Returns : std::vector<std::string>

The function should return a vector of strings. Each string should be a line of VLIW encoded instruction with masked encoding scheme
*/
std::vector<std::string> maskedVLIW(std::vector<std::string> instructions)
{
    return encode_instructions(instructions, format_masked_VLIW);
}

/*
Input : std::vector<std::string> instructions. The input is a vector of strings. Each string in the vector is one line in the vex code. A line can be a instruction or a ';;'

Returns : std::vector<std::string>

The function should return a vector of strings. Each string should be a line of VLIW encoded instruction with template encoding scheme
*/
std::vector<std::string> templateVLIW(std::vector<std::string> instructions)
{
    return encode_instructions(instructions, format_template_VLIW);
}


int main(int argc, char *argv[])
{

   if(argc != 2) {
       std::cout << "Invalid parameters \n";
       std::cout << "Expected use ./vliwEncoder <input file name>\n";
   }
 
   const char* inputFile = argv[1];
   const char* maskedOutput = "maskedEncoding.txt";
   const char* templateOutput = "templateEncoding.txt";

   std::vector<std::string> instructions;
   std::vector<std::string> maskedEncoding;
   std::vector<std::string> templateEncoding;
 
   /* Read instructions from the file */
   instructions = readInput(inputFile);

   /* Encode instructions using masked and template encoding */
   maskedEncoding = maskedVLIW(instructions);
   templateEncoding = templateVLIW(instructions);

   /* Print encoded instructions to file */
   printOutput(maskedOutput,maskedEncoding);
   printOutput(templateOutput,templateEncoding);
}

/*
 * predictive_text.c
 * predictive matching of words based upon partial strings
 */

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define END_OF_WORD_FLAG -1

typedef struct trienode {
    char ch;
    struct trienode* children[26 + 1]; // only do English alphabet, ignoring case and everything else
} trienode_t;

static trienode_t root;

void trie_insert(const char* word)
{
    trienode_t* curr = &root;
    for (int i = 0, n = strlen(word); i < n; i++) {
        if (!isalpha(word[i])) {
            continue;
        }
        char ch = tolower(word[i]);

        if (curr->children[ch-'a'] != NULL) {
            curr = curr->children[ch-'a'];
        } else {
            curr->children[ch-'a'] = calloc(1, sizeof(trienode_t));
            assert (curr->children[ch-'a'] != NULL);
            curr = curr->children[ch-'a'];
            curr->ch = ch;
        }
    }
    // make sure the NUL terminator is included in the trie to mark the word being finished!
    assert(curr != NULL);
    assert(curr != &root); // might not be smart?
    curr->children[26] = (void*)(intptr_t)END_OF_WORD_FLAG;
}

void load_words(const char* words_filename)
{
    FILE* fp = fopen(words_filename, "r");
    if (!fp) {
        perror(words_filename);
        exit(EXIT_FAILURE);
    }

    char line[BUFSIZ];
    while (fgets(line, sizeof(line), fp)) {
        trie_insert(line);
    }

    fclose(fp);
}

void find_words(trienode_t* p, char* buffer, const int pos, const size_t max)
{
    // base case - trie found the end-of-word flag
    if (p == (void*)(intptr_t)END_OF_WORD_FLAG) {
        buffer[pos] = '\0';
        printf("found word: %s\n", buffer);
        // todo: if we wanted to stash this in the output, we would give this 
        // function a state to copy the word into
    } else {
        buffer[pos] = p->ch;
        for (int i = 0; i < 27; i++) {
            if (p->children[i] != NULL) {
                find_words(p->children[i], buffer, pos+1, max);
            }
        }
    }
}

void predict_text(const char* prefix)
{
    printf("attempting to predict words for prefix %s\n", prefix);

    trienode_t* curr = &root;
    for (int i = 0, n = strlen(prefix); i < n && curr != NULL; i++) {
        if (!isalpha(prefix[i])) continue;
        char ch = tolower(prefix[i]);
        if (curr->children[ch-'a'] != NULL) {
            curr = curr->children[ch-'a'];
        } else {
            curr = NULL;
        }
    }

    // ok, prefix found! now dump the rest of the matching words
    if (curr != &root && curr != NULL) {
        char buffer[BUFSIZ];
        memset(buffer, '!', sizeof(buffer));
        strcpy(buffer, prefix);
        // -1 so we can let the recursive function start with the last letter
        // for keeping the implementation simpler
        find_words(curr, buffer, strlen(buffer)-1, sizeof(buffer));
    }
}

int main(int argc, char* argv[])
{
    int rc;
    if (argc < 2) {
        fprintf(stderr, "Usage: %s [word prefix]\n", argv[0]);
        rc = EXIT_FAILURE;
    } else {
        const char* prefix = argv[1];
        load_words("./words");
        predict_text(prefix);
        rc = EXIT_SUCCESS;
    }
    return rc;
}

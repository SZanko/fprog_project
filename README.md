# fprog_project

FIXME: description

## Installation

Download from http://example.com/FIXME.

## Usage

FIXME: explanation

    $ java -jar fprog_project-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2024 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.


# Project Description

For the problem: Please create a program that reads the large text file "War and Peace" by Tolstoy and inserts each unique word (without punctuation or numbers) into a persistent red-black tree. After reading and inserting each unique word into the balanced tree, the sorted list of words should be written into a file named "output.txt".

Refer to the implementation guidelines in Bartosz Milewski's article: Functional Data Structures in C++: Trees.

A nice non-functional implementation can be found here: https://github.com/yassiommi/redblacktree

Create your solution in C++, Haskell, F#, Java, Python, Groovy, Scala, or Lisp using the following steps

# Steps:

    Include Necessary Headers and Set Up the Main Function:
        Include headers for file I/O, strings, vectors, and other required data structures.
        Set up the main function where the program will run.

    Define the Red-Black Tree Data Structure:
        Implement the red-black tree data structure following the functional programming principles outlined in the referenced article. Ensure immutability and persistence.

    Read the Text File:
        Create a function to read the text file and return its content as a string or a vector of strings.

    Tokenize the Text:
        Create a function to tokenize the text into words. This function should handle punctuation and capitalization appropriately. Use functional programming techniques, such as higher-order functions and lambdas, for string manipulation and splitting.

    Insert Words into the Red-Black Tree:
        Create a function to insert each unique word into the red-black tree. This function should ensure that duplicates are not inserted and that the tree remains balanced. Use functional programming techniques and immutability.

    Traverse the Red-Black Tree:
        Create a function to traverse the red-black tree in order and collect the words in a sorted list. Use functional programming principles to ensure immutability and purity.

    Write the Sorted List to a File:
        Create a function to write the sorted list of words to "output.txt".

    Integrate All Functions:
        In the main function, integrate all the functions:
            Read and tokenize the text file.
            Insert each unique word into the red-black tree.
            Traverse the tree to get the sorted list of words.
            Write the sorted list to "output.txt".

    Ensure Immutability and Functional Principles:
        Review all functions and data structures to ensure they follow functional programming principles, such as immutability, purity, and the use of higher-order functions.

    Testing and Optimization:
        Test the program with the "War and Peace" text file.
        Optimize for performance, ensuring the red-black tree operations are efficient and the program can handle the large input size.
        Use doctest.h to test each function implemented.

Bonus Requirement (10% Extra Points): 

    Implement parallel execution to significantly improve performance (aim for at least double the speed). This can involve: 
        Parallelizing the tokenization of the text. 
        Parallelizing the insertion of words into the red-black tree. 
        Using functional programming techniques and libraries that support parallel execution.

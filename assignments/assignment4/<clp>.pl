% Question 1
% Part 1 (5 examples of removal of domain values)
% Note: we are using the grid notation outlined in the introduction here.
% for example, A1 is the top left corner, and I9 is the bottom right corner
%
% Example 1
% Let's look at I4
% We start with a domain of 123456789
% The numbers 135 are taken by other squares in row I
% We remove these from the domain to get 246789
% The numbers 12369 are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 478
% The numbers 123678 are taken by other squares in column 4
% We remove these from the domain to get 4
% Thus I4 must be 4 since we removed all other possible values from the domain

% Example 2
% Let's look at A4
% We start with a domain of 123456789
% The numbers 1359 are taken by other squares in row A
% We remove these from the domain to get 124678
% The numbers 23568 are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 147
% The numbers 123678 and 4 (see example 1) are taken by other squares in column 4
% We remove these from the domain to get 9
% Thus A4 must be 9 since we removed all other possible values from the domain

% Example 3
% Let's look at I6
% We start with a domain of 123456789
% The numbers 135 are taken by other squares in row I
% also, 4 is in the same row, see example 1
% We remove these from the domain to get 26789
% The numbers 12369 are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 78
% The numbers 235689 are taken by other squares in column 6
% These are removed from the domain to get 7
% Thus I6 must be 7 since we removed all other possible values from the domain

% Example 4
% Let's look at H5
% We start with a domain of 123456789
% The numbers 2389 are taken by other squares in row H
% We remove these from the domain to get 14567
% The numbers 12369 and 4 and 7 (see examples 1, 3) are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 5
% Thus H5 must be 5 since we removed all other possible values from the domain

% Example 5
% Let's look at G5
% We start with a domain of 123456789
% The numbers 2389 are taken by other squares in row G
% We remove these from the domain to get 14567
% The numbers 12369, and 4 and 7 and 5 (see examples 1, 3. 4) are taken by other squares in the 3x3 grid
% We remove these from the domain to get 8
% Thus G5 must be 8 since we removed all other possible values from the domain

% Question 1 part 2
% I went ahead and enumerated all of the possible values for all of the squares
% and found that it is indeed not possible to solve this puzzle using AC-3 alone.
% The closest you get using this algorithm, is that the domain for a few squares
% are reduced to only two possibilities. However, none of them can be reduced to
% just one possibility using AC-3 alone
% One obvious example of a domain value that cannot possibly be removed by AC-3
% is the number 4. By simply observing the puzzle, (and also by enumerating
% all of the possible values for each square), you quickly find out that the
% number 4 cannot ever be eliminated from any of the squares using just AC-3
% because none of the given squares contains the value 4. That is, in the initial
% state of the board, the number 4 never appears once in the entire board.
% So, there is no row, column, or 3x3 grid where the number 4 can be eliminated
% from the domain for a particular square, since is does not appear anywhere on
% the board initially. This alone does not prove that the number 4 cannot be
% removed from any square by AC-3 however. It would still be possible for the
% value 4 to be removed from the domain of a square, if some other square's
% domain could be reduced to the number 4. However, none of the squares in this
% case are reduced to a single value using AC-3 alone, so we are stuck with the
% given values, and none of those values is the number 4, so 4 cannot be removed
% from the domain of any square using just the AC-3 algorithm.

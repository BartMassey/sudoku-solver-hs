# Copyright © 2018 Bart Massey
# This work is made available under the "MIT License".
# Please see the file LICENSE in this distribution for
# license terms.

README.md: sudoku-solver.lhs
	./debird <sudoku-solver.lhs >README.md

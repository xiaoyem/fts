#
# Copyright (c) 2015 by Yuchao Zhao, Xiaoye Meng.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

require(evir)

da = read.table("data/d-csco9808.txt", header = T)
csco = log(1 + da[, 2])
# FIXME
par(mfcol = c(2, 1))
hill(csco,  option = c("xi"), end = 500)
hill(-csco, option = c("xi"), end = 500)
source("c7/Hill.R")
Hill(csco,  190)
Hill(-csco, 190)


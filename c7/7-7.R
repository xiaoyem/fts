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
da = read.table("data/d-aaspx9808.txt", header = TRUE)
head(da)
nsp=-log(da[,3]+1)*100
m1=gev(nsp,block=21)
m1
plot(m1)
par(mfcol=c(2,1))
qplot(nsp,threshold = 0.025,main='Negative daily SP log returns')
meplot(nsp)
title(mai='Mean excess plot')
mgpd=gpd(nsp,threshold = 0.025)
mgpd
par(mfcol=c(2,2))
plot(mgpd)
riskmeasures(mgpd,c(0.95,0.99,0.999))
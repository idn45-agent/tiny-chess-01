import json,os
import urllib.request as W
R=range
ND=(-33,-31,-18,-14,14,18,31,33)
BD=(-17,-15,15,17)
RD=(-16,-1,1,16)
KD=BD+RD
MT=NB=10**5
PS=[[0]*128 for _ in R(15)]
BV=(0,100,295,310,480,880,0)
CF=(0,0,7,5,0,3,0)
PA=(0,5,10,18,30,45,70,0)
for r in R(8):
 for f in R(8):
  s=r*16+f;c=min(f,7-f)+min(r,7-r)
  for t in R(1,6):PS[t][s]=BV[t]+CF[t]*c
  PS[1][s]+=PA[r]
  PS[4][s]+=18 if r==6 else 0
  PS[6][s]=(25,35,15,0,0,15,35,25)[f]-14*r if r<2 else 4*c-20
  for t in R(1,7):PS[t+8][s^112]=-PS[t][s]
KP={0:13,7:14,4:12,112:7,119:11,116:3}
DS=((ND,1),(BD,0),(RD,0),(KD,0),(KD,1))
CST=((1,(5,6),(4,5,6)),(2,(3,2,1),(4,3,2)))
B=[0]*128;S=C=E=0;K=[0,0];SC=nd=AB=LIM=0
def A(sq,by):
 b=B
 for D,w in((((-15,-17),(15,17))[by],1),(ND,2),(KD,6)):
  for d in D:
   if not sq+d&136 and b[sq+d]==w|by<<3:return 1
 for d in BD+RD:
  s=sq+d
  while not s&136:
   q=b[s]
   if q:
    if q>>3==by and q&7 in(5,3 if d in BD else 4):return 1
    break
   s+=d
def gen(cp):
 ms=[];ap=ms.append;b=B;tm=(S^1)<<3;pd=16-32*S
 for fr in R(128):
  p=b[fr]
  if not p or p>>3!=S:continue
  t=p&7
  if t==1:
   r=fr>>4;g=r==6-5*S;ts=[]
   if not cp and not b[fr+pd]:
    ts.append(fr+pd)
    if r==1+5*S and not b[fr+2*pd]:ap(fr|fr+2*pd<<7)
   for dd in(pd-1,pd+1):
    to=fr+dd
    if not to&136 and((b[to]and b[to]&8==tm)or(E and to==E)):ts.append(to)
   for to in ts:
    for pr in(5,4,3,2)if g else(0,):ap(fr|to<<7|pr<<14)
  else:
   D,lp=DS[t-2]
   for d in D:
    to=fr+d
    while not to&136 and not b[to]:
     if not cp:ap(fr|to<<7)
     if lp:break
     to+=d
    if not to&136 and b[to]and b[to]&8==tm:ap(fr|to<<7)
 if not cp:
  o=112*S
  for bit,em,pa in CST:
   if C&bit<<2*S and not any(b[s+o]for s in em)and not any(A(s+o,S^1)for s in pa):ap(K[S]|pa[-1]+o<<7)
 return ms
def mk(m):
 global S,C,E,SC
 fr=m&127;to=m>>7&127;pr=m>>14&7;p=B[fr];t=p&7;w=S;v=B[to];cs=to
 if t==1 and E and to==E and not v and fr&7!=to&7:cs=to+32*w-16;v=B[cs]
 u=(B[:],S,C,E,K[:],SC)
 d=-PS[p][fr]
 if v:d-=PS[v][cs];B[cs]=0
 q=pr|w<<3 if pr else p;B[to]=q;d+=PS[q][to];SC+=d
 B[fr]=0
 if t==6:
  K[w]=to
  if abs(to-fr)==2:
   rf,rt=(fr+3,fr+1)if to>fr else(fr-4,fr-1);rp=B[rf];B[rt]=rp;B[rf]=0;SC+=PS[rp][rt]-PS[rp][rf]
 C&=KP.get(fr,15)&KP.get(to,15)
 E=(fr+to)//2 if t==1 and abs(to-fr)==32 else 0
 S^=1
 if A(K[w],S):unmk(m,u);return
 return u
def unmk(m,u):
 global B,S,C,E,K,SC
 B,S,C,E,K,SC=u
def od(ms):
 ms.sort(key=lambda m:10*BV[B[m>>7&127]&7]+BV[m>>14&7]-(B[m&127]&7),reverse=True)
def sr(d,a,b,y):
 global nd,AB
 if y>90:return-SC if S else SC
 nd+=1
 if nd>LIM:AB=1;return 0
 chk=A(K[S],S^1);e=-MT-1
 if chk:d+=1
 elif d<1:
  e=-SC if S else SC
  if e>=b:return e
  if e>a:a=e
 ms=gen(d<1 and not chk);od(ms);L=0
 for m in ms:
  u=mk(m)
  if u is None:continue
  L+=1;s=-sr(d-1,-b,-a,y+1);unmk(m,u)
  if AB:return 0
  e=max(e,s);a=max(a,s)
  if a>=b:break
 if not L:return-MT+y if chk else e if d<1 else 0
 return e
def root():
 global nd,AB,LIM
 L=[]
 for m in gen(0):
  u=mk(m)
  if u:L.append(m);unmk(m,u)
 if len(L)<2:return L[0]
 od(L);e=L[0];nd=0;d=0
 while d<40:
  d+=1;LIM=1<<30 if d==1 else NB;a=-MT-1;L.remove(e);L.insert(0,e)
  for m in L:
   u=mk(m);s=-sr(d-1,-MT-1,-a,1);unmk(m,u)
   if AB:break
   if s>a:a=s;e=m
  if AB:AB=0;break
  if a>=MT-1000:break
 return e
def setup(fen,mv):
 global B,S,C,E,K,SC
 B=[0]*128;K=[0,0];SC=0;bd,sd,ct,ep,*_=fen.split();sq=112
 for ch in bd:
  if ch=="/":sq-=24
  elif ch.isdigit():sq+=int(ch)
  else:B[sq]="PNBRQK".index(ch.upper())+1+(8 if ch.islower()else 0);sq+=1
 S=sd!="w";C=sum(1<<i for i in R(4)if"KQkq"[i]in ct)
 E=0 if ep=="-"else(ord(ep[1])-49)*16+ord(ep[0])-97
 for sq in R(128):
  p=B[sq];SC+=PS[p][sq]
  if p&7==6:K[p>>3]=sq
def main():
 U=os.environ["MAKEFUN_ENV_URL"];P=lambda p,b:json.load(W.urlopen(W.Request(U+p,data=json.dumps(b).encode())))
 while 1:
  g=P("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   setup(g["fen"],[]);m=root();fr=m&127;to=m>>7&127;s=chr(97+(fr&7))+chr(49+(fr>>4))+chr(97+(to&7))+chr(49+(to>>4))
   if m>>14:s+=" pnbrqk"[m>>14&7]
   g=P("/move",{"game":g["game"],"move":s})
main()

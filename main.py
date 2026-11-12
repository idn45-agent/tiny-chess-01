import json,os,urllib.request as u
R=range
ND=[-33,-31,-18,-14,14,18,31,33]
BD=[-17,-15,15,17]
RD=[-16,-1,1,16]
KD=sorted(BD+RD)
MT=100000
PS=[[0]*128 for _ in R(15)]
SQ=[]
for r in R(8):
 for f in R(8):
  q=r*16+f;SQ.append(q);c=min(f,7-f)+min(r,7-r);PS[1][q]=100+[0,0,4,8,14,24,40,0][r]
  PS[2][q]=320+7*c-24;PS[3][q]=330;PS[4][q]=500;PS[5][q]=900;PS[6][q]=[20,25,10,0,0,10,25,20][f]if r==0 else max(-50,-15*r)
for q in SQ:
 for t in R(1,7):PS[t+8][q]=-PS[t][q^112]
KP={0:13,7:14,4:12,112:7,119:11,116:3}
CAST=((1,(1,2),(0,1,2)),(2,(-1,-2,-3),(0,-1,-2)))
B=[0]*128;S=0;C=0;E=0;K=[0,0];SC=0;nodes=0
def atk(q,y):
 b=B
 for D,ts,lp in((ND,4,1),(KD,64,1),(BD,40,0),(RD,48,0),((32*y-15,32*y-17),2,1)):
  for d in D:
   s=q+d
   while not s&136:
    x=b[s]
    if x:
     if x>>3==y and 1<<(x&7)&ts:return 1
     break
    if lp:break
    s+=d
 return 0
def gen(caps):
 ms=[];ap=ms.append;b=B
 for fr in SQ:
  p=b[fr]
  if not p or p>>3!=S:continue
  t=p&7
  if t==1:
   d=16-32*S;r=fr>>4
   def em(to):
    if r==6-5*S:
     for pr in R(2,6):ap(fr|to<<7|pr<<14)
    else:ap(fr|to<<7)
   if not caps and not b[fr+d]:
    em(fr+d)
    if r==1+5*S and not b[fr+2*d]:ap(fr|(fr+2*d)<<7)
   for dd in(d-1,d+1):
    to=fr+dd
    if not to&136 and(b[to]and b[to]>>3!=S or to==E!=0):em(to)
  else:
   D,leap=((ND,1),(BD,0),(RD,0),(KD,0),(KD,1))[t-2]
   for d in D:
    to=fr+d
    while not to&136:
     q=b[to]
     if q and q>>3!=S or not q and not caps:ap(fr|to<<7)
     if q or leap:break
     to+=d
   if t==6 and not caps:
    for bit,emp,path in CAST:
     if C&bit<<2*S and not any(b[fr+o]for o in emp)and not any(atk(fr+o,S^1)for o in path):ap(fr|(fr+path[-1])<<7)
 return ms
def mk(m):
 global S,C,E,SC;fr=m&127;to=m>>7&127;pr=m>>14&7;p=B[fr];t=p&7;w=S;v=B[to];csq=to
 if t==1 and to==E!=0 and not v and fr&7!=to&7:csq=to+32*w-16;v=B[csq]
 u=(B[:],S,C,E,K[:],SC)
 if v:
  SC-=PS[v][csq];B[csq]=0
 n=pr+(w<<3)if pr else p;B[to]=n;SC+=PS[n][to]-PS[p][fr]
 B[fr]=0
 if t==6:
  K[w]=to
  if abs(to-fr)==2:
   rf,rt=(fr+3,fr+1)if to>fr else(fr-4,fr-1);rp=B[rf];B[rt]=rp;B[rf]=0
   SC+=PS[rp][rt]-PS[rp][rf]
 C&=KP.get(fr,15)&KP.get(to,15)
 E=(fr+to)//2 if t==1 and abs(to-fr)==32 else 0
 S^=1
 if atk(K[w],S):unmk(m,u);return None
 return u
def unmk(m,u):
 global B,S,C,E,K,SC;B,S,C,E,K,SC=u
def od(ms):
 def k(m):
  v=B[m>>7&127]&7
  return 10*PS[v][100]+PS[m>>14&7][100]if v or m>>14&7 else 0
 ms.sort(key=k,reverse=True)
def sr(d,a,b,y):
 global nodes;nodes+=1
 if nodes>100000:1/0
 chk=atk(K[S],S^1)
 if d<=0:
  if y>90:return-SC if S else SC
  if chk:e=-MT-1
  else:
   e=-SC if S else SC
   if e>=b:return e
   if e>a:a=e
  ms=gen(not chk)
 else:
  if chk:d+=1
  e=-MT-1;ms=gen(False)
 od(ms)
 for m in ms:
  u=mk(m)
  if u is None:continue
  s=-sr(d-1,-b,-a,y+1);unmk(m,u)
  if s>e:e=s
  if s>a:a=s
  if a>=b:break
 if e==-MT-1:return-MT+y if chk else 0
 return e
def root():
 global nodes;L=[]
 for m in gen(False):
  u=mk(m)
  if u is not None:L.append(m);unmk(m,u)
 od(L);e=L[0];nodes=0;d=0
 while 1:
  d+=1;a=-MT-1;L.remove(e);L[:0]=[e]
  try:
   for m in L:
    u=mk(m);s=-sr(d-1,-MT-1,-a,1);unmk(m,u)
    if s>a:a=s;bm=m
  except:break
  e=bm
 return e
def sq2(u):return ord(u[1])*16+ord(u[0])-881
def setup(fen,moves):
 global B,S,C,E,K,SC;B=[0]*128;K=[0,0];SC=0;P=fen.split();q=112
 for ch in P[0]:
  if ch=="/":q-=24
  elif"0"<ch<":":q+=int(ch)
  else:
   t="PNBRQKpnbrqk".index(ch)+1;p=t if t<7 else t+2;B[q]=p;SC+=PS[p][q]
   if p%8==6:K[p>>3]=q
   q+=1
 S=P[1]!="w";C=0
 for i in R(4):
  if"KQkq"[i]in P[2]:C|=1<<i
 E=sq2(P[3])if P[3]!="-"else 0
 for u in moves:mk(sq2(u)|sq2(u[2:])<<7|(len(u)>4 and" pnbrqk".index(u[4]))<<14)
post=lambda a,b:json.load(u.urlopen(u.Request(os.getenv("MAKEFUN_ENV_URL")+a,data=json.dumps(b).encode())))
def main():
 while 1:
  g=post("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   setup(g["start_fen"],g["moves"]);r=root();m=chr(97+(r&7))+chr(49+(r>>4&7))+chr(97+(r>>7&7))+chr(49+(r>>11&7))+(r>>14 and" pnbrqk"[r>>14&7]or"")
   L=g["legal_moves"];g=post("/move",{"game":g["game"],"move":m if m in L else L[0]})
main()

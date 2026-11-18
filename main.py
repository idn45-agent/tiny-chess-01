import json,os,urllib.request as U
V=(0,100,320,330,500,900,0)
T=b"@@@@JJJJBBDFAABE@@@DA?>@ABB<@@@@68998<@@9@BC9ABD<@BD9ABC8<@A6899<>>>>@@@>@AB>AAB<ABB>BBB>@@@<>>>@@@@ABBB?@@@?@@@?@@@?@@@?@@@@@@A<>>?>@@@>@AA?@AA@@AA>AAA>@A@<>>?9886988698869886<998>???DD@@DFB@"
D=(14,18,31,33,-14,-18,-31,-33)
G=(1,-1,16,-16,15,17,-15,-17)
Z=(0,0,D,G[4:],G[:4],G,G)
R={0:2,7:1,112:8,119:4}
def att(q,y):
 for d in((y*-15,y*-17)+D):
  r=q+d
  if not r&136 and B[r]==(y if abs(d)in(15,17)else 2*y):return 1
 for i,d in enumerate(G):
  r=q+d
  while not r&136:
   p=B[r]
   if p:
    if p==5*y or p==y*(4-i//4)or(r==q+d and p==6*y):return 1
    break
   r+=d
 return 0
def setup(f,v):
 global B,S,C,E,K
 B=[0]*128;S=1;C=0;E=-1;K=[4,116]
 w=f.split();q=112;c=0
 for ch in w[0]:
  if ch=='/':q-=16;c=0
  elif ch.isdigit():c+=int(ch)
  else:
   p='PNBRQKpnbrqk'.index(ch);p=p+1 if p<6 else 5-p
   B[q+c]=p
   if abs(p)==6:K[p<0]=q+c
   c+=1
 S=2*(w[1]=='w')-1
 C=sum(1<<'KQkq'.index(c)for c in w[2]if c!='-')
 E=ord(w[3][0])-97+(ord(w[3][1])-49)*16 if w[3]!='-'else-1
def gen(cp):
 m=[]
 for f,p in enumerate(B):
  if f&136 or p*S<=0:continue
  t=abs(p)
  if t==1:
   d=16*S;r0=1+5*(S<0);rl=r0+6*S
   for e in(0,-1,1):
    q=f+d+e
    if q&136:continue
    v=B[q]
    s=16*(abs(v)or 1)-1 if e and(v*S<0 or q==E)else 0 if not e and not v and(not cp or q>>4==rl)else-1
    if s<0:continue
    for z in((5,4,3,2)if q>>4==rl else(0,)):m.append(f|q<<7|z<<14|s<<17)
    if not e and q>>4!=rl and f>>4==r0 and not B[f+2*d]:m.append(f|f+2*d<<7)
  else:
   ds=Z[t]
   for d in ds:
    q=f+d
    while not q&136:
     v=B[q]
     if v*S<0 or(not v and not cp):m.append(f|q<<7|(16*abs(v)-t)*(v*S<0)<<17)
     if v or t not in(3,4,5):break
     q+=d
 if not cp:
  o=112*(S<0);h=o+4
  if B[h]==6*S:
   for kt in(6,2):
    b=2-kt//4;rf=7*(kt==6);a=2+kt//2
    if C&b<<2*(S<0)and B[o+rf]==4*S and not(B[o+a]or B[o+kt]or(kt==2 and B[o+1]))and not any(att(s,-S)for s in(h,o+a,o+kt)):m.append(h|o+kt<<7)
 return m
def mk(m,u=0):
 global S,C,E
 f=m&127;t=m>>7&127
 rf=t+1 if t>f else t-2;rt=t-1 if t>f else t+1
 if u:
  pc,vc,E,C,K[0],K[1],ep=u
  S=-S;B[f]=pc;B[t]=vc
  if ep>=0:B[ep]=-S
  if abs(pc)==6 and abs(t-f)==2:B[rf]=B[rt];B[rt]=0
  return
 p=m>>14&7
 pc=B[f];vc=B[t];ep=-1
 if abs(pc)==1 and t==E and not vc:
  ep=t-16*S;B[ep]=0
 u=(pc,vc,E,C,K[0],K[1],ep)
 B[t]=p*S if p else pc;B[f]=0
 if abs(pc)==6:
  K[1-(S>0)]=t
  if abs(t-f)==2:B[rt]=B[rf];B[rf]=0
  C&=12 if S>0 else 3
 for x in(f,t):C&=~R.get(x,0)
 E=f+16*S if abs(pc)==1 and abs(t-f)==32 else-1
 S=-S
 if att(K[S>0],S):
  unmk(m,u);return None
 return u
unmk=mk
def ev():
 s=0
 for q,p in enumerate(B):
  if q&136 or not p:continue
  t=abs(p);r=q>>4;f=q&7
  v=V[t]+5*(T[(t-1)*32+(r if p<0 else 7-r)*4+min(f,7-f)]-64)
  s+=v if p>0 else-v
 return s if S>0 else-s
def sr(d,a,b,p):
 global N,F
 if(N:=N+1)>100000:F=1;return 0
 ck=att(K[1-(S>0)],-S)
 if d<=0:
  if p>12:return ev()
  if not ck:
   v=ev()
   if v>=b:return b
   if v>a:a=v
 elif ck and p<20:d+=1
 ms=gen(d<=0 and not ck)
 n=0
 for m in sorted(ms,reverse=True):
  u=mk(m)
  if u is None:continue
  n+=1
  v=-sr(max(d-1,0),-b,-a,p+1)
  unmk(m,u)
  if v>=b:return b
  if v>a:a=v
 if not n:return-9999+p if ck else a*(d<=0)
 return a
def rt():
 global N,F
 N=0;F=0
 ms=gen(False);bm=None;d=1
 while d<32 and not F:
  a=-9999;bs=a;cb=None
  ms.sort(key=lambda m:(m!=bm,-(m>>17)))
  for m in ms:
   u=mk(m)
   if u is None:continue
   v=-sr(d-1,-9999,-a,1)
   unmk(m,u)
   if F:break
   if v>bs:bs=v;cb=m
   if v>a:a=v
  if not F:bm=cb or bm
  d+=1
 return bm
def uci(m):
 f=m&127;t=m>>7&127;p=m>>14&7
 s='%c%c%c%c'%(97+(f&7),49+(f>>4),97+(t&7),49+(t>>4))
 return s+'nbrq'[p-2] if p else s
def post(p,b):
 return json.loads(U.urlopen(U.Request(os.environ['MAKEFUN_ENV_URL']+p,data=json.dumps(b).encode())).read())
def main():
 while 1:
  g=post('/game',{})
  if g['status'][0]=='d':break
  while g['status'][0]=='y':
   setup(g['fen'],[])
   lm=g['legal_moves'];u=uci(rt()or 0)
   if u not in lm:u=lm[0]
   g=post('/move',{'game':g['game'],'move':u})
main()

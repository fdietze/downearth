#version 120
#extension GL_EXT_gpu_shader4 : enable

varying vec4 worldpos;

int rshift(int x, int y) {return int(float(uint(x)) / pow(2f,float(uint(y))));}
int lshift(int x, int y) {return int(float(uint(x)) * pow(2f,float(uint(y))));}

//def hash(k:Int) = (((k*0x12345678) &gt;&gt;&gt; (k*0x87754351))^seed) & 0x7FFFFFFF
//int hash(int k) { return int(float(k)*12436234f)%1332427; }
//int hash(int k) { return ((k*int(0x12345678)) >> (  (k*int(0x87754351))&31  ) ) & int(0x7FFFFFFF); }

// universal hash function:
int hash(int k) {return ((0x1345452*k) % 1332427) % 0x93564; }

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }
float grad(int hash, float x, float y, float z) {
      int h = hash % 16;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h%2) == 0 ? u : -u) + ((h%4-h%2) == 0 ? v : -v);
}

float noise3(float x, float y, float z) {
	int X = fastfloor(x);
	int Y = fastfloor(y);
	int Z = fastfloor(z);

	float relx = x - float(X);
	float rely = y - float(Y);
	float relz = z - float(Z);

	float u = fade(relx);
	float v = fade(rely);
	float w = fade(relz);
	
	int A = hash(X  )+Y; int AA = hash(A)+Z; int AB = hash(A+1)+Z;
	int	B = hash(X+1)+Y; int BA = hash(B)+Z; int BB = hash(B+1)+Z;

	return lerp(w,	lerp(v,	lerp(u, grad(hash(AA  ), relx  , rely  , relz	),
									grad(hash(BA  ), relx-1, rely  , relz	)),
							lerp(u, grad(hash(AB  ), relx  , rely-1, relz	),
									grad(hash(BB  ), relx-1, rely-1, relz	))),
					lerp(v, lerp(u, grad(hash(AA+1), relx  , rely  , relz-1 ),
									grad(hash(BA+1), relx-1, rely  , relz-1 )),
							lerp(u, grad(hash(AB+1), relx  , rely-1, relz-1 ),
									grad(hash(BB+1), relx-1, rely-1, relz-1 ))));
}

void main(){
	vec4 hashcolor;
	float color = (noise3(worldpos.x, worldpos.y, worldpos.z)+1)/2;
    gl_FragColor = vec4(color,color,color,1.0);
}



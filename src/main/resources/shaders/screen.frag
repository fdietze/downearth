 
#version 120
//#extension GL_EXT_gpu_shader4 : enable
//#extension GL_ARB_gpu_shader_fp64 : enable

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;
uniform float time;

/*
possible vertex shader:
#version 120

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

void main () {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

	vertex = vec3(gl_ModelViewMatrix * gl_Vertex);       
	normal = normalize(gl_NormalMatrix * gl_Normal);
    world  = gl_Vertex;
}
*/

/*int seed = 0;
int a = (seed ^ int(0xB5C18E6A)) | ((1 << 16) + 1);
int c = seed ^ int(0xF292D0B2);
int hash(int x){ return (a*(x ^ c)) >> 16; }
int hash(int k) { return ((k*int(0x12345678)) >> (k*int(0x87754351))) & 0x7FFFFFFF; }*/
int hash(int k) { return int(mod(((k*34)+1)*k, 289)); }

/*float grad(int hash, float x, float y, float z) {
      int h = hash & 0xF;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
}*/
float grad(int hash, float x, float y, float z) {
      int h = int(mod(hash,16));
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((mod(h,2)) == 0 ? u : -u) + ((mod(h,4)-mod(h,2)) == 0 ? v : -v);
}

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }

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

float noise3(vec3 v) {return noise3(v.x, v.y, v.z);}

/////////////////////////////////////////////////////

float multiplyconstantexp_uid4e563a9c(float a, float value) {return a*value;}
vec4 matrgb_uid4e563a9c(float r, float g, float b) {return vec4(r, g, b, 0.0);}
float perlinnoise3_uid4e563a9c(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
vec4 matmix_uid4e563a9c(vec4 m1, float t, vec4 m2, float shift) {return t >= shift ? m1 : m2;}
vec3 scalesrcv_uid4e563a9c(float scale) {return world.xyz * scale;}
float timeseconds_uid4e563a9c() {return time;}



void main(){

float vn10_timeseconds_uid4e563a9c = timeseconds_uid4e563a9c();
float vn5_multiplyconstantexp_uid4e563a9c = multiplyconstantexp_uid4e563a9c(vn10_timeseconds_uid4e563a9c, 0.2642545101403451);
vec3 vn9_scalesrcv_uid4e563a9c = scalesrcv_uid4e563a9c(0.18946457081379972);
float vn7_perlinnoise3_uid4e563a9c = perlinnoise3_uid4e563a9c(vn9_scalesrcv_uid4e563a9c, 0.0, 0.0, 0.0, vn5_multiplyconstantexp_uid4e563a9c, 0.0, 0.4600938253124378, 2.173469725052115, 0.0);
vec4 vn2_matrgb_uid4e563a9c = matrgb_uid4e563a9c(0.37, 0.22, 0.66);
float vn3_perlinnoise3_uid4e563a9c = perlinnoise3_uid4e563a9c(vn9_scalesrcv_uid4e563a9c, vn7_perlinnoise3_uid4e563a9c, vn7_perlinnoise3_uid4e563a9c, vn7_perlinnoise3_uid4e563a9c, 0.0, 0.0, 1.0, 0.8010698775896219, 0.0);
vec4 vn4_matrgb_uid4e563a9c = matrgb_uid4e563a9c(0.4, 0.5, 0.95);
vec4 vn1_matmix_uid4e563a9c = matmix_uid4e563a9c(vn4_matrgb_uid4e563a9c, vn3_perlinnoise3_uid4e563a9c, vn2_matrgb_uid4e563a9c, 0.24852523575870045);


	vec4 materialcolor = vn1_matmix_uid4e563a9c;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}

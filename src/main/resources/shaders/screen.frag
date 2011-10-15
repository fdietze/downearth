 
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

float addconstantexp_uid4e7cbecf(float a, float value) {return a+value;}
vec4 matrgb_uid4e7cbecf(float r, float g, float b) {return vec4(r, g, b, 1);}
float scalesrcy_uid4e7cbecf(float scale) {return world.y * scale;}
vec4 matstone_uid4e7cbecf() {return vec4(0.56, 0.56, 0.56, 0.0);}
float scalesrcx_uid4e7cbecf(float scale) {return world.x * scale;}
vec4 matmix_uid4e7cbecf(vec4 m1, float t, vec4 m2, float shift) {return t >= shift ? m1 : m2;}
float perlinnoise3_uid4e7cbecf(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
float sum3_uid4e7cbecf(float a, float b, float c) {return a+b+c;}
float scalesrcz_uid4e7cbecf(float scale) {return world.z * scale;}
vec3 scalesrcv_uid4e7cbecf(float scale) {return world.xyz * scale;}



void main(){

vec3 vn2_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0);
float vn2_scalesrcy_uid4e7cbecf = scalesrcy_uid4e7cbecf(1.0);
float vn2_scalesrcx_uid4e7cbecf = scalesrcx_uid4e7cbecf(1.0);
float vn11_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn2_scalesrcv_uid4e7cbecf, 0.0, 0.0, 0.0, 0.0, 0.0, 6.588728138140584, 1.7411011265922491, 0.0);
float vn9_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vec3(0), vn2_scalesrcx_uid4e7cbecf, vn2_scalesrcy_uid4e7cbecf, 0.0, 0.0, 0.0, 0.10881882041201557, 0.18946457081379972, 0.0);
float vn2_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0);
float vn12_sum3_uid4e7cbecf = sum3_uid4e7cbecf(vn2_scalesrcz_uid4e7cbecf, vn9_perlinnoise3_uid4e7cbecf, vn11_perlinnoise3_uid4e7cbecf);
float vn8_scalesrcz_uid4e7cbecf = scalesrcz_uid4e7cbecf(1.0);
vec3 vn8_scalesrcv_uid4e7cbecf = scalesrcv_uid4e7cbecf(1.0);
vec4 vn13_matrgb_uid4e7cbecf = matrgb_uid4e7cbecf(0.84, 0.49, 0.0);
float vn5_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vec3(0), 0.0, 0.0, vn12_sum3_uid4e7cbecf, 0.0, 0.0, 5.278031643091579, 0.12158186842653569, -0.12);
vec4 vn15_matrgb_uid4e7cbecf = matrgb_uid4e7cbecf(0.62, 0.28, 0.0);
float vn6_perlinnoise3_uid4e7cbecf = perlinnoise3_uid4e7cbecf(vn8_scalesrcv_uid4e7cbecf, 0.0, 0.0, 0.0, 0.0, vn8_scalesrcz_uid4e7cbecf, 0.05006686734935137, 4.228072162245522, -0.62);
vec4 vn14_matmix_uid4e7cbecf = matmix_uid4e7cbecf(vn15_matrgb_uid4e7cbecf, vn5_perlinnoise3_uid4e7cbecf, vn13_matrgb_uid4e7cbecf, 0.0);
float vn10_addconstantexp_uid4e7cbecf = addconstantexp_uid4e7cbecf(vn6_perlinnoise3_uid4e7cbecf, 5.597467536876814);
vec4 vn3_matstone_uid4e7cbecf = matstone_uid4e7cbecf();
vec4 vn17_matmix_uid4e7cbecf = matmix_uid4e7cbecf(vn3_matstone_uid4e7cbecf, vn10_addconstantexp_uid4e7cbecf, vn14_matmix_uid4e7cbecf, 0.0);


	vec4 materialcolor = vn17_matmix_uid4e7cbecf;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}

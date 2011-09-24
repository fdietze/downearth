 
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

float sum3_uid4e7ca7b6(float a, float b, float c) {return a+b+c;}
float perlinnoise3_uid4e7ca7b6(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
vec4 matmix_uid4e7ca7b6(vec4 m1, float t, vec4 m2, float shift) {return t >= shift ? m1 : m2;}
vec4 matrgb_uid4e7ca7b6(float r, float g, float b) {return vec4(r, g, b, 1);}
float addconstantexp_uid4e7ca7b6(float a, float value) {return a+value;}
float scalesrcz_uid4e7ca7b6(float scale) {return world.z * scale;}
float scalesrcx_uid4e7ca7b6(float scale) {return world.x * scale;}
vec3 scalesrcv_uid4e7ca7b6(float scale) {return world.xyz * scale;}
float scalesrcy_uid4e7ca7b6(float scale) {return world.y * scale;}
vec4 matstone_uid4e7ca7b6() {return vec4(0.56, 0.56, 0.56, 0.0);}



void main(){

vec3 vn2_scalesrcv_uid4e7ca7b6 = scalesrcv_uid4e7ca7b6(1.0);
float vn2_scalesrcy_uid4e7ca7b6 = scalesrcy_uid4e7ca7b6(1.0);
float vn2_scalesrcx_uid4e7ca7b6 = scalesrcx_uid4e7ca7b6(1.0);
float vn15_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vn2_scalesrcv_uid4e7ca7b6, 0.0, 0.0, 0.0, 0.0, 0.0, 6.588728138140584, 1.7411011265922491, 0.0);
float vn6_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vec3(0), vn2_scalesrcx_uid4e7ca7b6, vn2_scalesrcy_uid4e7ca7b6, 0.0, 0.0, 0.0, 0.10881882041201557, 0.18946457081379972, 0.0);
float vn2_scalesrcz_uid4e7ca7b6 = scalesrcz_uid4e7ca7b6(1.0);
float vn17_sum3_uid4e7ca7b6 = sum3_uid4e7ca7b6(vn2_scalesrcz_uid4e7ca7b6, vn6_perlinnoise3_uid4e7ca7b6, vn15_perlinnoise3_uid4e7ca7b6);
float vn4_scalesrcz_uid4e7ca7b6 = scalesrcz_uid4e7ca7b6(1.0);
vec3 vn4_scalesrcv_uid4e7ca7b6 = scalesrcv_uid4e7ca7b6(1.0);
vec4 vn3_matrgb_uid4e7ca7b6 = matrgb_uid4e7ca7b6(0.84, 0.49, 0.0);
float vn1_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vec3(0), 0.0, 0.0, vn17_sum3_uid4e7ca7b6, 0.0, 0.0, 5.278031643091579, 0.12158186842653569, -0.12);
vec4 vn7_matrgb_uid4e7ca7b6 = matrgb_uid4e7ca7b6(0.62, 0.28, 0.0);
float vn12_perlinnoise3_uid4e7ca7b6 = perlinnoise3_uid4e7ca7b6(vn4_scalesrcv_uid4e7ca7b6, 0.0, 0.0, 0.0, 0.0, vn4_scalesrcz_uid4e7ca7b6, 0.05006686734935137, 4.228072162245522, -0.62);
vec4 vn14_matmix_uid4e7ca7b6 = matmix_uid4e7ca7b6(vn7_matrgb_uid4e7ca7b6, vn1_perlinnoise3_uid4e7ca7b6, vn3_matrgb_uid4e7ca7b6, 0.0);
float vn8_addconstantexp_uid4e7ca7b6 = addconstantexp_uid4e7ca7b6(vn12_perlinnoise3_uid4e7ca7b6, 5.597467536876814);
vec4 vn5_matstone_uid4e7ca7b6 = matstone_uid4e7ca7b6();
vec4 vn13_matmix_uid4e7ca7b6 = matmix_uid4e7ca7b6(vn5_matstone_uid4e7ca7b6, vn8_addconstantexp_uid4e7ca7b6, vn14_matmix_uid4e7ca7b6, 0.0);


	vec4 materialcolor = vn13_matmix_uid4e7ca7b6;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}

 
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

vec4 matrgb_uid4e4b916c(float r, float g, float b) {return vec4(r, g, b, 0.0);}
float perlinnoise3_uid4e4b916c(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
float scalesrcx_uid4e4b916c(float scale) {return world.x * scale;}
float timeseconds_uid4e4b916c() {return time;}
float addconstantexp_uid4e4b916c(float a, float value) {return a+value;}
float scalesrcz_uid4e4b916c(float scale) {return world.z * scale;}
float min2_uid4e4b916c(float a, float b) {return min(a,b);}
vec3 scalevec3_uid4e4b916c(vec3 v, float x, float y, float z) {return v*vec3(x,y,z);}
vec4 matmix_uid4e4b916c(vec4 m1, float t, vec4 m2, float shift) {return t >= shift ? m1 : m2;}
float negate_uid4e4b916c(float a) {return -a;}
vec3 scalesrcv_uid4e4b916c(float scale) {return world.xyz * scale;}
vec4 matthreshold_uid4e4b916c(vec4 m1, float t, vec4 m2) {return t >= 0 ? m1 : m2;}
float scalesrcy_uid4e4b916c(float scale) {return world.y * scale;}
vec3 createvec3_uid4e4b916c(float x, float y, float z) {return vec3(x,y,z);}
float sphere_uid4e4b916c(vec3 v, float radius) {return radius - sqrt(dot(v,v));}



void main(){

float vn3_scalesrcz_uid4e4b916c = scalesrcz_uid4e4b916c(0.10881882041201557);
float vn17_addconstantexp_uid4e4b916c = addconstantexp_uid4e4b916c(vn3_scalesrcz_uid4e4b916c, 54.19169999120173);
float vn8_addconstantexp_uid4e4b916c = addconstantexp_uid4e4b916c(vn17_addconstantexp_uid4e4b916c, 0.16957554093095903);
float vn3_scalesrcy_uid4e4b916c = scalesrcy_uid4e4b916c(0.10881882041201557);
float vn3_scalesrcx_uid4e4b916c = scalesrcx_uid4e4b916c(0.10881882041201557);
vec3 vn23_createvec3_uid4e4b916c = createvec3_uid4e4b916c(vn3_scalesrcx_uid4e4b916c, vn3_scalesrcy_uid4e4b916c, vn8_addconstantexp_uid4e4b916c);
vec3 vn24_scalevec3_uid4e4b916c = scalevec3_uid4e4b916c(vn23_createvec3_uid4e4b916c, 0.7169776240079135, 0.7169776240079135, 22.315898661606493);
float vn16_sphere_uid4e4b916c = sphere_uid4e4b916c(vn23_createvec3_uid4e4b916c, 54.19169999120173);
float vn10_timeseconds_uid4e4b916c = timeseconds_uid4e4b916c();
vec3 vn5_scalesrcv_uid4e4b916c = scalesrcv_uid4e4b916c(1.0);
float vn2_sphere_uid4e4b916c = sphere_uid4e4b916c(vn24_scalevec3_uid4e4b916c, 131.59856981197643);
vec3 vn3_scalesrcv_uid4e4b916c = scalesrcv_uid4e4b916c(0.10881882041201557);
float vn7_addconstantexp_uid4e4b916c = addconstantexp_uid4e4b916c(vn16_sphere_uid4e4b916c, 30.124958317193155);
float vn15_sphere_uid4e4b916c = sphere_uid4e4b916c(vn23_createvec3_uid4e4b916c, 38.85423630064148);
float vn31_perlinnoise3_uid4e4b916c = perlinnoise3_uid4e4b916c(vn5_scalesrcv_uid4e4b916c, 0.0, 0.0, 0.0, vn10_timeseconds_uid4e4b916c, 0.0, 0.18946457081379972, 1.3947436663504058, 0.0);
float vn22_perlinnoise3_uid4e4b916c = perlinnoise3_uid4e4b916c(vn3_scalesrcv_uid4e4b916c, 0.0, 0.0, 0.0, vn2_sphere_uid4e4b916c, 0.0, 0.7169776240079135, 256.0, -0.56);
float vn19_negate_uid4e4b916c = negate_uid4e4b916c(vn7_addconstantexp_uid4e4b916c);
vec4 vn12_matrgb_uid4e4b916c = matrgb_uid4e4b916c(1.0, 0.34, 0.0);
float vn26_perlinnoise3_uid4e4b916c = perlinnoise3_uid4e4b916c(vn5_scalesrcv_uid4e4b916c, vn31_perlinnoise3_uid4e4b916c, vn31_perlinnoise3_uid4e4b916c, vn31_perlinnoise3_uid4e4b916c, vn15_sphere_uid4e4b916c, 0.0, 0.4600938253124378, 10.26740718050323, -0.7);
vec4 vn18_matrgb_uid4e4b916c = matrgb_uid4e4b916c(1.0, 0.56, 0.0);
float vn30_min2_uid4e4b916c = min2_uid4e4b916c(vn19_negate_uid4e4b916c, vn22_perlinnoise3_uid4e4b916c);
vec4 vn21_matrgb_uid4e4b916c = matrgb_uid4e4b916c(0.53, 0.35, 0.2);
vec4 vn25_matthreshold_uid4e4b916c = matthreshold_uid4e4b916c(vn18_matrgb_uid4e4b916c, vn26_perlinnoise3_uid4e4b916c, vn12_matrgb_uid4e4b916c);
vec4 vn6_matrgb_uid4e4b916c = matrgb_uid4e4b916c(0.48, 0.57, 0.03);
float vn13_addconstantexp_uid4e4b916c = addconstantexp_uid4e4b916c(vn30_min2_uid4e4b916c, 22.315898661606493);
vec4 vn14_matrgb_uid4e4b916c = matrgb_uid4e4b916c(0.59, 0.51, 0.42);
vec4 vn4_matrgb_uid4e4b916c = matrgb_uid4e4b916c(0.0, 0.13, 0.7);
float vn1_perlinnoise3_uid4e4b916c = perlinnoise3_uid4e4b916c(vn3_scalesrcv_uid4e4b916c, 0.0, 0.0, 0.0, vn16_sphere_uid4e4b916c, 0.0, 0.05593906693299827, 0.2642545101403451, 0.0);
vec4 vn9_matthreshold_uid4e4b916c = matthreshold_uid4e4b916c(vn25_matthreshold_uid4e4b916c, vn15_sphere_uid4e4b916c, vn21_matrgb_uid4e4b916c);
vec4 vn11_matthreshold_uid4e4b916c = matthreshold_uid4e4b916c(vn14_matrgb_uid4e4b916c, vn13_addconstantexp_uid4e4b916c, vn6_matrgb_uid4e4b916c);
vec4 vn29_matmix_uid4e4b916c = matmix_uid4e4b916c(vn9_matthreshold_uid4e4b916c, vn1_perlinnoise3_uid4e4b916c, vn4_matrgb_uid4e4b916c, -0.11737425948457414);
vec4 vn20_matmix_uid4e4b916c = matmix_uid4e4b916c(vn29_matmix_uid4e4b916c, vn16_sphere_uid4e4b916c, vn11_matthreshold_uid4e4b916c, -0.11737425948457414);


	vec4 materialcolor = vn20_matmix_uid4e4b916c;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}

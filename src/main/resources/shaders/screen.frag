 
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

float sum2_uid4e56524d(float a, float b) {return a+b;}
float scalesrcz_uid4e56524d(float scale) {return world.z * scale;}
vec3 createvec3_uid4e56524d(float x, float y, float z) {return vec3(x,y,z);}
float scalesrcy_uid4e56524d(float scale) {return world.y * scale;}
vec4 matmix_uid4e56524d(vec4 m1, float t, vec4 m2, float shift) {return t >= shift ? m1 : m2;}
float scalesrcx_uid4e56524d(float scale) {return world.x * scale;}
float sphere_uid4e56524d(vec3 v, float radius) {return radius - sqrt(dot(v,v));}
float timeseconds_uid4e56524d() {return time;}
vec4 matrgb_uid4e56524d(float r, float g, float b) {return vec4(r, g, b, 0.0);}
float min2_uid4e56524d(float a, float b) {return min(a,b);}
float addconstantexp_uid4e56524d(float a, float value) {return a+value;}
float negate_uid4e56524d(float a) {return -a;}
vec3 scalevec3_uid4e56524d(vec3 v, float x, float y, float z) {return v*vec3(x,y,z);}
vec4 matthreshold_uid4e56524d(vec4 m1, float t, vec4 m2) {return t >= 0 ? m1 : m2;}
float perlinnoise3_uid4e56524d(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
vec3 scalesrcv_uid4e56524d(float scale) {return world.xyz * scale;}



void main(){

float vn2_scalesrcz_uid4e56524d = scalesrcz_uid4e56524d(0.040107059298840744);
float vn5_addconstantexp_uid4e56524d = addconstantexp_uid4e56524d(vn2_scalesrcz_uid4e56524d, 31.124958317193155);
float vn3_addconstantexp_uid4e56524d = addconstantexp_uid4e56524d(vn5_addconstantexp_uid4e56524d, 0.16957554093095903);
float vn2_scalesrcy_uid4e56524d = scalesrcy_uid4e56524d(0.040107059298840744);
float vn2_scalesrcx_uid4e56524d = scalesrcx_uid4e56524d(0.040107059298840744);
vec3 vn12_createvec3_uid4e56524d = createvec3_uid4e56524d(vn2_scalesrcx_uid4e56524d, vn2_scalesrcy_uid4e56524d, vn3_addconstantexp_uid4e56524d);
vec3 vn14_scalevec3_uid4e56524d = scalevec3_uid4e56524d(vn12_createvec3_uid4e56524d, 0.7169776240079135, 0.7169776240079135, 22.315898661606493);
float vn26_sphere_uid4e56524d = sphere_uid4e56524d(vn12_createvec3_uid4e56524d, 31.124958317193155);
float vn19_timeseconds_uid4e56524d = timeseconds_uid4e56524d();
vec3 vn13_scalesrcv_uid4e56524d = scalesrcv_uid4e56524d(1.0);
float vn28_sphere_uid4e56524d = sphere_uid4e56524d(vn14_scalevec3_uid4e56524d, 256.0);
vec3 vn2_scalesrcv_uid4e56524d = scalesrcv_uid4e56524d(0.040107059298840744);
float vn6_addconstantexp_uid4e56524d = addconstantexp_uid4e56524d(vn26_sphere_uid4e56524d, 9.26740718050323);
float vn7_sphere_uid4e56524d = sphere_uid4e56524d(vn12_createvec3_uid4e56524d, 24.933266549136007);
float vn17_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn13_scalesrcv_uid4e56524d, 0.0, 0.0, 0.0, vn19_timeseconds_uid4e56524d, 0.0, 0.18946457081379972, 1.3947436663504058, 0.0);
float vn15_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn2_scalesrcv_uid4e56524d, 0.0, 0.0, 0.0, vn28_sphere_uid4e56524d, 0.0, 0.23651441168139897, 256.0, -0.72);
float vn16_negate_uid4e56524d = negate_uid4e56524d(vn6_addconstantexp_uid4e56524d);
vec4 vn23_matrgb_uid4e56524d = matrgb_uid4e56524d(1.0, 0.34, 0.0);
float vn29_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn13_scalesrcv_uid4e56524d, vn17_perlinnoise3_uid4e56524d, vn17_perlinnoise3_uid4e56524d, vn17_perlinnoise3_uid4e56524d, vn7_sphere_uid4e56524d, 0.0, 0.4600938253124378, 10.26740718050323, -0.7);
vec4 vn11_matrgb_uid4e56524d = matrgb_uid4e56524d(1.0, 0.56, 0.0);
float vn1_min2_uid4e56524d = min2_uid4e56524d(vn16_negate_uid4e56524d, vn15_perlinnoise3_uid4e56524d);
float vn8_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn2_scalesrcv_uid4e56524d, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.32987697769322366, 0.0);
float vn9_perlinnoise3_uid4e56524d = perlinnoise3_uid4e56524d(vn2_scalesrcv_uid4e56524d, 0.0, 0.0, 0.0, vn26_sphere_uid4e56524d, 0.0, 0.08717147914690036, 0.21168632809063176, 0.0);
vec4 vn4_matrgb_uid4e56524d = matrgb_uid4e56524d(0.53, 0.35, 0.2);
vec4 vn32_matthreshold_uid4e56524d = matthreshold_uid4e56524d(vn11_matrgb_uid4e56524d, vn29_perlinnoise3_uid4e56524d, vn23_matrgb_uid4e56524d);
vec4 vn21_matrgb_uid4e56524d = matrgb_uid4e56524d(0.48, 0.57, 0.03);
float vn24_addconstantexp_uid4e56524d = addconstantexp_uid4e56524d(vn1_min2_uid4e56524d, 22.315898661606493);
vec4 vn30_matrgb_uid4e56524d = matrgb_uid4e56524d(0.56, 0.51, 0.48);
vec4 vn20_matrgb_uid4e56524d = matrgb_uid4e56524d(0.0, 0.13, 0.7);
float vn10_sum2_uid4e56524d = sum2_uid4e56524d(vn9_perlinnoise3_uid4e56524d, vn8_perlinnoise3_uid4e56524d);
vec4 vn22_matthreshold_uid4e56524d = matthreshold_uid4e56524d(vn32_matthreshold_uid4e56524d, vn7_sphere_uid4e56524d, vn4_matrgb_uid4e56524d);
vec4 vn31_matthreshold_uid4e56524d = matthreshold_uid4e56524d(vn30_matrgb_uid4e56524d, vn24_addconstantexp_uid4e56524d, vn21_matrgb_uid4e56524d);
vec4 vn18_matmix_uid4e56524d = matmix_uid4e56524d(vn22_matthreshold_uid4e56524d, vn10_sum2_uid4e56524d, vn20_matrgb_uid4e56524d, -0.11737425948457414);
vec4 vn27_matmix_uid4e56524d = matmix_uid4e56524d(vn18_matmix_uid4e56524d, vn26_sphere_uid4e56524d, vn31_matthreshold_uid4e56524d, -0.11737425948457414);


	vec4 materialcolor = vn27_matmix_uid4e56524d;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}

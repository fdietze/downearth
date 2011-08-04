#version 120
#extension GL_EXT_gpu_shader4 : enable
//#extension GL_ARB_gpu_shader_fp64 : enable

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

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

int seed = 0;
int a = (seed ^ int(0xB5C18E6A)) | ((1 << 16) + 1);
int c = seed ^ int(0xF292D0B2);
int hash(int x){ return (a*(x ^ c)) >> 16; }

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }

float grad(int hash, float x, float y, float z) {
      int h = hash & 0xF;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
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

float noise3(vec3 v) {return noise3(v.x, v.y, v.z);}

/////////////////////////////////////////////////////

vec4 result(float d, vec4 m) {return m;}
vec3 scalesrcv(float scale) {return world.xzy * scale;}
vec4 matthreshold(vec4 m1, float t, vec4 m2) {return t > 0 ? m1 : m2;}
float summedinputnoise3(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
float scalesrcy(float scale) {return (world.z - 150) * scale;}
vec4 matgold() {return vec4(0.98, 0.71, 0.08, 0.0);}
vec4 matstone() {return vec4(0.56, 0.56, 0.56, 0.0);}
vec4 matgravel() {return vec4(0.31, 0.31, 0.31, 0.0);}
vec4 matearth() {return vec4(0.35, 0.22, 0.06, 0.0);}



void main(){
vec3 vn9_scalesrcv = scalesrcv(0.895);
float vn16_summedinputnoise3 = summedinputnoise3(vn9_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.16957554093095892, 3.0314331330207955, 0.0);
vec4 vn6_matstone = matstone();
float vn13_summedinputnoise3 = summedinputnoise3(vn9_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1517743605493808, 1.248330548901612, -0.18000000000000005);
vec4 vn7_matgravel = matgravel();
vec4 vn4_matgold = matgold();
float vn15_summedinputnoise3 = summedinputnoise3(vn9_scalesrcv, vn16_summedinputnoise3, vn16_summedinputnoise3, vn16_summedinputnoise3, 0.0, 0.0, 0.05593906693299827, 1.0, 0.6200000000000001);
vec4 vn12_matthreshold = matthreshold(vn7_matgravel, vn13_summedinputnoise3, vn6_matstone);
float vn9_scalesrcy = scalesrcy(3.7842305869023836);
float vn11_summedinputnoise3 = summedinputnoise3(vn9_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.12158186842653576, 3.7842305869023836, 0.0);
vec4 vn14_matthreshold = matthreshold(vn12_matthreshold, vn15_summedinputnoise3, vn4_matgold);
float vn10_summedinputnoise3 = summedinputnoise3(vn9_scalesrcv, vn11_summedinputnoise3, vn11_summedinputnoise3, vn11_summedinputnoise3, vn9_scalesrcy, 0.0, 0.025737219289611674, 8.224910613248527, 0.0);
vec4 vn8_matearth = matearth();
vec4 vn3_matthreshold = matthreshold(vn8_matearth, vn10_summedinputnoise3, vn14_matthreshold);
vec4 vn1_result = result(0.0, vn3_matthreshold);


	vec4 materialcolor = vn1_result;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}



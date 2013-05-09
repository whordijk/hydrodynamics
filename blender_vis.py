import numpy as no
import bpy

def loadData(fName,N):
    f = open(fName)
    N=f.readline()
    N = int(N.split()[1])

    positions = np.loadtxt(f,unpack=True)
    positions = np.reshape(positions,(3,N,-1),order='F')
    
    return positions

ob = bpy.data.objects.new(obName, me)
def run(origo):
    origin = Vector(origo)
    (x,y,z) = (0.707107, 0.258819, 0.965926)
    verts = ((x,x,-1), (x,-x,-1), (-x,-x,-1), (-x,x,-1), (0,0,1))
    faces = ((1,0,4), (4,2,1), (4,3,2), (4,0,3), (0,1,2,3))





    cone3 = createMeshFromPrimitive('PrimCone', origin+Vector((0,4,0)))
    ps.mesh.primitive_cone_add(
        vertices=4, 
        radius=1, 
        depth=1, 
        cap_end=True, 
        view_align=False, 
        enter_editmode=False, 
        location=origin, 
        rotation=(0, 0, 0))

    return

if __name__ == "__main__":
         
    run((0,0,0))

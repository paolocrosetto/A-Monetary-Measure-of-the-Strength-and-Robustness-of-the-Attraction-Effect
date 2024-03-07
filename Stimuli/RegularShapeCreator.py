#!/usr/bin/python
# Regular Shape Drawer

# How it works:
# 1. A function for each regular polygon (tri,sq,pent,goldenrect,circ,goldenellipse)
# 2. Shapes are centered using Polygon library bounding box
# 3. Shapes are written to .SVG file
#
# All you actually have to do is to change the list of areas in the 'do' line at the bottom of the file
# Note: Area must be <= 0.5 (no triangle can have an area >0.5 in a (1,1) bounding box)

import svgfig                                 #creating .svg files
from Polygon import Polygon                   #used to easily create bounding boxes of non-rect polygons
from math import pi, sqrt, cos, sin, radians  #area calculations
import os                                     #creating directories and the like

def center_anypol(anypol):
    """Function to center a shape on its bounding box"""
    bbox = Polygon(anypol).boundingBox()
    bboxcentre = (0.5*(bbox[0]+bbox[1]), 0.5*(bbox[2]+bbox[3]))
    newpol = [0]*len(anypol) 
    for i in range(len(anypol)):
        newpol[i] = (anypol[i][0]-(bboxcentre[0]-0.5), anypol[i][1]-(bboxcentre[1]-0.5))
    return newpol

def gen_eqtri(ar):
    """Generates a list of 3 tuples defining a regular triangle of area 'ar'."""
    sd = sqrt(4*ar/sqrt(3))
    return [(0,0),(sd,0),(sd/2,(2*ar)/sd)]

def gen_eqpent(ar):
    """Generates a list of 5 tuples defining a regular pentagon of area 'ar'."""
    sd = sqrt(ar/1.720477401)
    return[(0.15,0),(0.15+sd,0),(0.15+sd+sd*cos(radians(72)),sd*sin(radians(72))),((0.30+sd)/2, sd*sin(radians(36))+sd*sin(radians(72))),(0.15-sd*cos(radians(72)), sd*sin(radians(72)))]

def gen_goldrect(ar):
    """Generates a list of 4 tuples defining a golden rectangle (long=1.618*short) of area 'ar'."""
    corto = sqrt((2/(1+sqrt(5)))*ar)
    lungo = corto*1.618034
    return [(0,0),(lungo,0),(lungo,corto),(0,corto)]

def gen_square(ar):
    """Generates a list of 4 tuples defining a square of area 'ar'."""
    side = sqrt(ar)
    return [(0,0),(side,0),(side,side),(0,side)]        

def gen_circle(ar):
    """Generates a list of tuples defining a svgfig 'ellipse', with R=r=radius and area 'ar'."""
    radius = sqrt(ar/pi)
    return (0.5,0.5,radius,0,radius)

def gen_goldell(ar):
    """Generates a list of tuples defining a svgfig 'ellipse', with R=r*1.618 and area 'ar'."""
    corto = sqrt(ar/(pi*1.618033988))
    lungo = corto*1.618034
    return (0.5,0.5, lungo,0,corto)

def gen_poly(ar):
    """Creates and centres polygons, returns list"""
    tri = center_anypol(gen_eqtri(ar))
    sq = center_anypol(gen_square(ar))
#    pent = center_anypol(gen_eqpent(ar))
#    rect = center_anypol(gen_goldrect(ar))
    return [tri,sq]#,pent,rect]

def gen_cir(ar):
    """Creates and centres circles, returns list"""
    cir = gen_circle(ar)
#    ell = gen_goldell(ar)
    return [cir]#,ell]
    
def draw_poly(ar):
    """Draws polygons to file"""
    i = 0
    for shape in gen_poly(ar):
        if i == 0:
            name = 'tri'
        else:
            name = 'sq'
        a = svgfig.Poly(shape, loop=True, fill='grey', stroke='grey')
#        a = svgfig.Poly(shape, loop=True, stroke='black')        
        b = svgfig.Grid(0,1,0,1, ticks=12, stroke='#CFD1EC')
        c = svgfig.Fig(b,a)
        c.SVG(svgfig.window(0,1,0,1)).save('%s_%s.svg'%(name,'%0.3f'%ar))  
#        a.SVG(svgfig.window(0,1,0,1)).save('%s_%s.svg'%(name,'%0.3f'%ar))  
        i +=1

def draw_cir(ar):
    """Draws ellipses to file"""
    for shape in gen_cir(ar):
        name = 'cir'
        a = svgfig.Ellipse(shape[0],shape[1],shape[2],shape[3],shape[4], fill='grey', stroke='grey')
#        a = svgfig.Ellipse(shape[0],shape[1],shape[2],shape[3],shape[4], stroke='black')
        b = svgfig.Grid(0,1,0,1, ticks=12, stroke='#CFD1EC')
        c = svgfig.Fig(b,a)
        c.SVG(svgfig.window(0,1,0,1)).save('%s_%s.svg'%(name,'%0.3f'%ar))
#        a.SVG(svgfig.window(0,1,0,1)).save('%s_%s.svg'%(name,'%0.3f'%ar))
              
def do(arlist):
    """Main function"""
    for ar in arlist:
        draw_poly(ar)
        draw_cir(ar)
    for f in os.listdir(os.curdir):
        if f[-3:] == 'svg':
            a = open(f)
            b = a.read()
            b = b.replace('stroke-width="0.25pt"', 'stroke-width="0.05pt"')
            a.close()
            a = open(f,'w')
            a.write(b)

def sizes():
    import random
    from numpy import arange
    b = list(arange(0.100,0.440,0.03))
#    if len(b)== 14: b.append(0.425)
#    else: pass
#    #this part randomizes a bit the sequence (I do not think it is needed)
#    if len(b)== 14: b.append(0.425)
#    else: pass
#    c = [random.normalvariate(0,0.01) for i in range(13)]
#    for i in range(1,13):
#        b[i] = b[i]+c[i-1]
    return b
    
if __name__ == '__main__':
    try:
        os.mkdir('Shapes')
    except:
        pass
    os.chdir('Shapes')
    do(sizes())
    
def roundup(x):
    if round(x,1)>x: return round(x,1)
    else: return round(x,1)+0.1

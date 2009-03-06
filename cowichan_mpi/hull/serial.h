/**
 * Serial implementation of convex hull
 *
 * \file serial.h
 * \author Andrew Borzenko
 * \date 03-06-09
 */

#pragma once
#ifndef SERIAL_H
#define SERIAL_H

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

void hull (pt1D* points,     // list of points
           int n,            // number of points
           pt1D* hullPoints, // list of points in convex hull
           int* hn);         // number of points in convex hull

/*--------------------------------------------------------------*/
/* private functions						*/
/*--------------------------------------------------------------*/

void split (pt1D* points,    // list of points
           int n,            // number of points
           pt1D* hullPoints, // list of points in convex hull
           int* hn,          // number of points in convex hull
           pt min_p,         // point with minimum x
           pt max_p);        // point with maximum x

#endif /* SERIAL_H */

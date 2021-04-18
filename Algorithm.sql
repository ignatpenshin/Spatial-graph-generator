--delete from panoramas.angmin;
--delete from panoramas.turns;
--delete from panoramas.panoramas_rmax;

--drop function  panoramas.Rmax(integer, double precision);
CREATE OR REPLACE FUNCTION panoramas.Rmax(region_iid integer, max_dist double precision)
    RETURNS  void
    LANGUAGE 'plpgsql'
 
AS $BODY$
 
DECLARE
 	r1 record;
    r2 record;
BEGIN
For r1 IN 
select * from panoramas.panoramas c where c.region = region_iid
	LOOP
    For r2 IN 
		SELECT b.iid, b.region, b.geom FROM panoramas.panoramas a, panoramas.panoramas b--, panoramas.buildings_wgs c, panoramas.osm_roads d
		where a.iid = r1.iid and ST_Distance(a.geom::geography , b.geom::geography ) < max_dist and ST_Distance(a.geom, b.geom) != 0
		and a.region = region_iid and b.region = region_iid
		EXCEPT
		SELECT b.iid, b.region, b.geom FROM panoramas.panoramas a, panoramas.panoramas b,/* panoramas.buildings_wgs c, */panoramas.osm_roads d
		where a.iid = r1.iid and ST_Distance(a.geom::geography , b.geom::geography ) < max_dist and ST_Distance(a.geom, b.geom) != 0
		and a.region = region_iid and b.region = region_iid
		and ST_Intersects(ST_Buffer(a.geom, 0.001), d.geom) = true
		and ST_Intersects(ST_MakeLine(st_force2D(a.geom), st_force2D(b.geom)), d.geom) = true
		EXCEPT
		SELECT b.iid, b.region, b.geom FROM panoramas.panoramas a, panoramas.panoramas b, panoramas.buildings_wgs c--, panoramas.osm_roads d
		where a.iid = r1.iid and ST_Distance(a.geom::geography , b.geom::geography ) < max_dist and ST_Distance(a.geom, b.geom) != 0
		and a.region = region_iid and b.region = region_iid
		and ST_Intersects(ST_Buffer(a.geom, 0.001), c.geom) = true
		and ST_Intersects(ST_MakeLine(st_force2D(a.geom), st_force2D(b.geom)), c.geom) = true group by b.iid, b.region, b.geom
		
		LOOP
			insert into panoramas.panoramas_rmax (iid1, iid2, track_id, geom1, distance, geom2) values (r1.iid, r2.iid, r2.region, 
							r1.geom,
						(select ST_Distance(a.geom::geography, b.geom::geography) FROM panoramas.panoramas a, panoramas.panoramas b
					WHERE a.iid = r1.iid and b.iid = r2.iid), r2.geom);
		END LOOP;
 	END LOOP;	       
END
$BODY$;
 
ALTER FUNCTION panoramas.Rmax(integer, double precision)
    OWNER TO postgres;	
	
--select * from panoramas.Rmax(51, 100)	
	

--drop function  panoramas.ANGmin(integer);
CREATE OR REPLACE FUNCTION panoramas.ANGmin(region_iid integer)
    RETURNS  void
    LANGUAGE 'plpgsql'
 
AS $BODY$
 
DECLARE
    r1 record;
    r2 record;
	r3 record;
BEGIN
For r1 in 
	select a.iid, a.geom from panoramas.panoramas a where a.region = region_iid
	LOOP
    For r2 IN 
        select b.iid2, b.distance, b.geom2 from panoramas.panoramas_rmax b where b.iid1 = r1.iid 
        LOOP
            For r3 IN 
                select b.iid2, b.distance, b.geom2 from panoramas.panoramas_rmax b where b.iid1 = r1.iid 
                LOOP
				insert into panoramas.angmin (iid1, iid2, distance1, distance2, smallang, iid) values (r2.iid2, r3.iid2, r2.distance, r3.distance,
						(select degrees(ST_Azimuth(b.geom, a.geom)) - degrees(ST_Azimuth(b.geom, c.geom))
					FROM panoramas.panoramas a, 
                panoramas.panoramas b, panoramas.panoramas c
                        where a.iid = r2.iid2 and b.iid = r1.iid and c.iid = r3.iid2), r1.iid);
                 END LOOP;
        END LOOP;
    END LOOP; 
END
$BODY$;
 
ALTER FUNCTION panoramas.ANGmin(integer)
    OWNER TO postgres;  
	
--select * from panoramas.ANGmin(51)		
	
--drop function  panoramas.excepts(double precision);
CREATE OR REPLACE FUNCTION panoramas.excepts(excepts_ang double precision)
    RETURNS  void
    LANGUAGE 'plpgsql'
 
AS $BODY$
 
DECLARE
    r1 record;
BEGIN
    For r1 IN 
	  select * from panoramas.angmin where (@smallang < excepts_ang or @smallang > (360 - excepts_ang)) and smallang <> 0.00000000 
        LOOP
			IF r1.distance1 < r1.distance2
			THEN
			delete from panoramas.panoramas_rmax a where a.iid1 = r1.iid and a.iid2 = r1.iid2;
			ELSE 
			delete from panoramas.panoramas_rmax a where a.iid1 = r1.iid and a.iid2 = r1.iid1;
			END IF;
         END LOOP; 
END
$BODY$;
 
ALTER FUNCTION panoramas.excepts(double precision)
    OWNER TO postgres;  	
	
-- select * from panoramas.excepts(60)	

--drop function  panoramas.filter(integer, double precision);
CREATE OR REPLACE FUNCTION panoramas.filter(region_iid integer, filter_ang double precision)
Returns TABLE (srcpano integer, dstpano integer, angle double precision, isgood boolean)
LANGUAGE 'plpgsql'
 
AS $BODY$
 
DECLARE
	r4 record;
	r5 record;
	r6 record;
	ang double precision;
	good boolean;
BEGIN
for r4 in 
		select a.srcpano from panoramas.turns a, panoramas.panoramas b where a.srcpano = b.iid and b.region = region_iid  group by a.srcpano
		LOOP
		for r5 in 
			select b.dstpano from panoramas.turns b, panoramas.panoramas c where b.srcpano = r4.srcpano and b.dstpano = c.iid and c.region = region_iid
			LOOP 
			For r6 in
				select iid2, distance from panoramas.panoramas_Rmax where iid1 = r4.srcpano order by distance --limit 6 
				LOOP
					ang:= degrees(ST_Azimuth(b.geom, a.geom)) - degrees(ST_Azimuth(b.geom, c.geom)) 
					FROM panoramas.panoramas a, panoramas.panoramas b, panoramas.panoramas c
            		where a.iid = r5.dstpano and b.iid = r4.srcpano and c.iid = r6.iid2;
					IF (@ang > filter_ang  and  @ang < (360 - filter_ang)) or ang = 0
					THEN good:= True;
					ELSE
					good:= False;
					EXIT WHEN good = False;
					END IF;
					RETURN NEXT; 
       			END LOOP;
		   IF good = False
	   	   THEN 
		   srcpano:= r4.srcpano;
		   dstpano:= r5.dstpano;
		   isgood:= good;
	       END IF;
           RETURN NEXT;		
		END LOOP;
		RETURN NEXT;		
	END LOOP;			
RETURN;	
END
$BODY$;
 
ALTER FUNCTION panoramas.filter(integer, double precision)
    OWNER TO postgres;	

--drop function  panoramas.filter2(integer, double precision);
CREATE OR REPLACE FUNCTION panoramas.filter2(region_iid integer, filter2_ang double precision)
Returns TABLE (srcpano integer, dstpano integer)
LANGUAGE 'plpgsql'
 
AS $BODY$
 
DECLARE
	r4 record;
	r5 record;
	r6 record;
	ang double precision;
	distance_1 double precision; 
	distance_2 double precision;
BEGIN
for r4 in 
		select a.srcpano from panoramas.turns a, panoramas.panoramas b 
		where a.srcpano = b.iid and b.region = region_iid  group by a.srcpano
		LOOP
		for r5 in 
			select b.dstpano from panoramas.turns b, panoramas.panoramas c 
			where b.srcpano = r4.srcpano and b.dstpano = c.iid and c.region = region_iid
			LOOP 
			For r6 in
				select c.dstpano from panoramas.turns c, panoramas.panoramas e 
				where c.srcpano = r4.srcpano and c.dstpano = e.iid and e.region = region_iid
				LOOP
					ang:= degrees(ST_Azimuth(b.geom, a.geom)) - degrees(ST_Azimuth(b.geom, c.geom)) 
					FROM panoramas.panoramas a, panoramas.panoramas b, panoramas.panoramas c
            		where a.iid = r5.dstpano and b.iid = r4.srcpano and c.iid = r6.dstpano;
					IF 
					(@ang < filter2_ang or @ang > (360 - filter2_ang)) and ang != 0 
					and exists (select a.srcpano from panoramas.turns a 
								where a.srcpano = r5.dstpano and a.dstpano = r6.dstpano)			
					THEN 
						distance_1:= ST_Distance(a.geom::geography, b.geom::geography) FROM panoramas.panoramas a, panoramas.panoramas b
						WHERE a.iid = r4.srcpano and b.iid = r5.dstpano;
						distance_2:= ST_Distance(a.geom::geography, b.geom::geography) FROM panoramas.panoramas a, panoramas.panoramas b
						WHERE a.iid = r4.srcpano and b.iid = r6.dstpano;
						IF distance_1 < distance_2
						THEN
						srcpano:= r4.srcpano;
						dstpano:= r6.dstpano;
						ELSE 
						srcpano:= r4.srcpano;
						dstpano:= r5.dstpano;
						END IF;
					END IF;
					RETURN NEXT; 
       			END LOOP;
           RETURN NEXT;		
		END LOOP;
		RETURN NEXT;		
	END LOOP;			
RETURN;	
END
$BODY$;
 
ALTER FUNCTION panoramas.filter2(integer, double precision)
    OWNER TO postgres;	

--drop function  panoramas.fullturn(integer, double precision, double precision, double precision, double precision);
CREATE OR REPLACE FUNCTION panoramas.fullturn(region_iid integer, max_dist double precision, 
											  excepts_ang double precision, filter_ang double precision, filter2_ang double precision)
Returns void
LANGUAGE 'plpgsql'
 
AS $BODY$
 
DECLARE
    r1 record;
	r2 record;
	r3 record;
	r4 record;
	r5 record;
BEGIN
	delete from panoramas.angmin;
	delete from panoramas.panoramas_rmax;
	perform panoramas.rmax(region_iid, max_dist);
	perform panoramas.angmin(region_iid);
	perform panoramas.excepts(excepts_ang);
	For r2 in 
		select iid from panoramas.panoramas k where k.region = region_iid
		LOOP
		For r1 in
			select iid1, iid2, geom1, geom2 from panoramas.panoramas_rmax r where r.iid1 = r2.iid order by distance limit 4
				LOOP
				insert into panoramas.turns (srcpano, dstpano, geom) values (r1.iid1, r1.iid2, 
						(select ST_MakeLine(st_force2D(r1.geom1), st_force2D(r1.geom2))));				
				END LOOP;
		END LOOP;		
	For r3 in
		select a.iid, a.srcpano, a.dstpano from panoramas.turns a	
		except
		select a.iid, a.srcpano, a.dstpano from panoramas.turns a, panoramas.turns b 
		where a.srcpano = b.dstpano and b.srcpano = a.dstpano 
			LOOP
				insert into panoramas.turns (srcpano, dstpano, geom) values (r3.dstpano, r3.srcpano, 
						(select ST_MakeLine(st_force2D(a.geom), st_force2D(b.geom)) 
								from panoramas.panoramas a, panoramas.panoramas b where a.iid = r3.dstpano and b.iid = r3.srcpano));				
			END LOOP;
	For r5 in 
		select g.srcpano, g.dstpano from panoramas.filter2(region_iid, filter2_ang) g
		group by g.srcpano, g.dstpano order by g.srcpano
		Loop
			Delete from panoramas.turns d where d.srcpano = r5.srcpano and d.dstpano = r5.dstpano; 
			Delete from panoramas.turns d where d.srcpano = r5.dstpano and d.dstpano = r5.srcpano; 			
		END LOOP;		
	For r4 in 
		select f.srcpano, f.dstpano from panoramas.filter(region_iid, filter_ang) f
		group by f.srcpano, f.dstpano order by f.srcpano   
		Loop
			Delete from panoramas.turns d where d.srcpano = r4.srcpano and d.dstpano = r4.dstpano; 
			Delete from panoramas.turns d where d.srcpano = r4.dstpano and d.dstpano = r4.srcpano; 				
		END LOOP;	
END
$BODY$;
 
ALTER FUNCTION panoramas.fullturn(integer, double precision, double precision, double precision, double precision)
    OWNER TO postgres;	
			
select panoramas.fullturn(9999, 100, 40, 40, 40);
--select panoramas.delturn(3724)
--select panoramas.delreg(50)
--delete from panoramas.turns;
--update panoramas.panoramas SET region = 51 where track_id = 4358

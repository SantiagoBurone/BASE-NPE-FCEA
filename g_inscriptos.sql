REM -- ********************************************************************************* --
REM --  Listado de ingresos a facultad entre las fechas pasadas como parametro indicando --
REM --  CI, nombre, carrera, ciclo, fecha de ingreso y observaciones.                    --
REM --                                                                                   --
REM --  Total de inscriptos entre esas fechas agrupados por carrera.                     --
REM --                                                                                   --
REM -- ********************************************************************************* --

spool d:/seciu/salidas/g_inscriptos.lst

set pages 64 
set lines 132
set ver off
def fech1=&fecha1
def fech2=&fecha2

ttitle 'LISTADO DE INSCRIPTOS A FACULTAD ENTRE LAS FECHAS &fech1 Y &fech2 '

break on e.estci
select e.estci , e.nomest NOMBRE, i.carr CARR, i.ciclo CICLO, i.fecing FECHA, i.obs
from estudiantes e, icarr i
where i.fecing between to_number(&fech1) and to_number(&fech2) and
      i.estci = e.estci
order by e.nomest;
ttitle off;

ttitle 'TOTAL DE INSCRIPTOS POR CARRERA ENTRE LAS FECHAS &fech1 Y &fech2'
select carr, count(distinct estci) ESTUDIANTES
from icarr 
where fecing between to_number(&fech1) and to_number(&fech2)
group by carr;

ttitle off;
spool out;
set ver on;

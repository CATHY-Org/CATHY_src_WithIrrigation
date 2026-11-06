% WatertableZ: computes watertable from inputfiles xyz and psi,
%              watertable at depth (d)
% Open the output file dtcoupling
global BESTX BESTF ICALL PX PF

save('tmp.mat');
clear all;
% close all;

% surface node to plot
surfnod2plot = [62 64 66 69]; % from 57 to 70 are the columns of 1 row
surfnod2plot = 64; % closest one to account for spatial installation sensor

dtcoupling = fopen('../output/dtcoupling','r');
%Read in the file
a=0;
nstep=0;
ATMDIAGN=[];
while (a~=(-1))              % Find out number of lines and timesteps (nstep)
    a = fgetl(dtcoupling);   % Loops trough dtcoupling until a=-1 end of file
    nstep = nstep+1;
end
a=0;
nstep = nstep-31; % the first 28 lines of file are text and last 3 rows of file are text
%nstep = nstep-(26+1); % Number of timesteps
frewind(dtcoupling);
%for i = 1:24
for i = 1:28
    fgets(dtcoupling);
end
ATMDIAGN = fscanf(dtcoupling,'%g',[22,nstep]); % Read data
ATMDIAGN = ATMDIAGN';
%close ('all');
fclose(dtcoupling);
jmax=max(ATMDIAGN(:,11));
timeatm(1)=ATMDIAGN(1,3);
for i = 2:nstep
    timeatm(i)=timeatm(i-1)+ATMDIAGN(i-1,2);
end

xyz = fopen('../output/xyz','r');

%psi = fopen('../output/sw_3','r'); # import as following
% line=1 in file to be removed
fid = fopen('../output/sw', 'r') ;              % Open source file.
fgetl(fid) ;                                  % Read/discard line.
buffer = fread(fid, Inf) ;                    % Read rest of the file.
fclose(fid);
fid = fopen('../output/sw_truncated', 'w')  ;   % Open destination file.
fwrite(fid, buffer) ;                         % Save to file.
fclose(fid) ;

psi = fopen('../output/sw_truncated','r');


% Soil parameter porosity
fid = fopen('../input/soil', 'r') ;              % Open source file.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
fgetl(fid) ;                                  % Read/discard line.
buffer = fread(fid, Inf) ;                    % Read rest of the file.
fclose(fid);
fid = fopen('../input/soil_truncated', 'w')  ;   % Open destination file.
fwrite(fid, buffer) ;                         % Save to file.
fclose(fid) ;

% Read in data from psi
myporosity = fopen('../input/soil_truncated','r');
PHI = fscanf(myporosity,'%g %g %g %g %g %g %g %g'); %, [8 20]
fclose(myporosity);
delete('../input/soil_truncated');
PHI = PHI(5:8:size(PHI,1),1); % 5 is starting column porosity, 8 is n columns of soil file
% assume that the index selected corresponds to zdepth, and top node is
% ATM, so "not" in soil file
% PHI would be actually per nnod
% PHI = [PHI; PHI(size(PHI,1))];

% Read in output files
% xyz
a=0;
b=0;
c=0;
minx=0;
miny=0;
maxx=0;
maxy=0;
nstr=0;                             % Number of node layers
XYZsurface=[];
Z=[];

nnod = fscanf(xyz,'%u %u',2);       % Read header
nstr = nnod(2)/nnod(1);             % nnod=[(number of surface nodes) (number of nodes)]
fgets(xyz);                         % Skip line
XYZ = fscanf(xyz,'%g',[4,nnod(2)]); % Read data
XYZ = XYZ';
XYZsurface=XYZ(1:nnod(1),:);
for a=0:nstr-1                      % Create Z, contains elevation of each node.
    b=a+1;                          % Vertical profiles in rows, so that the lowest
    for c=1:nnod(1)                 % nodes are in column 1.
        Z(c,b)=XYZ(a*nnod(1)+c,4);
    end
end
Z = fliplr(Z);
fclose(xyz);
maxx=max(XYZsurface(:,2));
maxy=max(XYZsurface(:,3));
minx=min(XYZsurface(:,2));
miny=min(XYZsurface(:,3));

% psi
b=0;
c=0;
d=0;
nprt=0;          % Number of time values for detailed nodal output
minz00=NaN;
minz00all=inf;
maxz00=NaN;
maxz00all=-inf;
length=0;
width=0;
gridsize=0;
rc=0;
z0=0;
i=[];
TIME=[];
PSI=[];
VPPSI=[];
Z0=[];
% L3=[];
% L11=[];
% L3NOD=[1707 1708 1815 1816 2478 2479 2594 2595 1383 1384 1485 1486 1914 1915 2023 2024 3262 3263 3375 3376 2587 2588 2703 2704];
% L3NOD=[1707 1708 1815 1816 2478 2479 2594 2595 1383 1384 1485 1486 3262 3263 3375 3376 2587 2588 2703 2704];
% L11NOD=[694 695 756 757 1702 1703 1810 1811 3369 3370 3481 3482 3145 3146 3258 3259 3813 3814 3922 3923];
% L11NOD=[694 695 756 757 1702 1703 1810 1811 3369 3370 3481 3482 3145 3146 3258 3259 3813 3814 3922 3923];
% L3NOD=[1706 1707 1708 1814 1815 1816 1923 1924 1925]; % Water table in the selected locations
% L3NOD=[L3NOD 3031 3032 3033 3145 3146 3147 3258 3259 3260];
%L11NOD=[3031 3032 3033 3145 3146 3147 3258 3259 3260];
%L3NOD=[15919 13188 16446 17247 10449 13392 7844 19565 5554]; % Water table in the selected locations
%L3NOD=[9 10 177 178];
% [m1 n1]=size(L3NOD);
% [m2 n2]=size(L11NOD);
Z00=[];                % FLAG, flag for what has happened
SizeZ00=[];            % 1 watertable calculated correctly
Z00ZEROS=[];           % 2 unstaturated zone above saturated zone above unsaturated zone
FILENAME=[];           % 3 watertable not encountered, fully saturated vertical profile
FLAG=[];               % 4 watertable not encountered, unsaturated profile
CATCHMENT=[];
CONTOURLEVELS=[];

while (b~=(-1))      % Find out number of time values for detailed nodal output (nprt)
    b = fgetl(psi);
    if (b~=(-1))     % Loops trough psi until b=-1 end of file
        c = double(b);
    end
    %if c(29:33)==[78 83 84 69 80] % c(29:33)='NSTEP'
    if size(c)==[1 40]
        if c(29:33)==[78 83 84 69 80] % c(29:33)='NSTEP'
            nprt = nprt+1;
        end
    end
end
frewind(psi);

FLAG = zeros(nnod(1),nprt);

% mymat = zeros(size(surfnod2plot,2), nprt,nstr);
mymat = zeros(nprt,nstr);

for a=1:nprt % nprt will be 2 more because considers automatically beginning and end
    % Z0 contains for every vertical profile the height of the watertable,
    % default is the watertable at highest node
    Z0 = XYZsurface(:,4);
    time = fscanf(psi,'%u %g',[1,2]);
    TIME = [TIME;time]; % TIME contains timestep and times of nprt
    sbarr=[TIME(a,2) 0];
    fgets(psi);
    % Read in data from psi
    PSI = fscanf(psi,'%g',[(nstr*nnod(1)),1]);
    for b=0:nstr-1        % Create VPPSI, contains psi of each node.
        c=b+1;            % Vertical profiles in rows and layers in columns,
        for d=1:nnod(1)   % so that the bottom layer is in clolumn 1.
            VPPSI(d,c)=PSI(b*nnod(1)+d);
        end
    end
    % VPPSI = fliplr(VPPSI);

    mymat(a,:) = VPPSI(surfnod2plot,:); % pick a surface node to plot
    % for aa=1:size(surfnod2plot,2)
    %     mymat(aa,a,:) = VPPSI(surfnod2plot(aa),:); % pick a surface node to plot
    % end
end

fclose(psi);
delete('../output/sw_truncated');

% a=linspace(0,200,400)'*3600; % sampling times from CATHY in seconds to plot soil moisture profiles
% a=linspace(360,460,200)'*3600; % sampling times from CATHY in seconds to plot soil moisture profiles
% a=linspace(650,750,200)'*3600; % sampling times from CATHY in seconds to plot soil moisture profiles

% 0 1 2.0
%    0.025 0.025 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.05 0.1
% -> 0.05  0.1   0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0  1.1  1.2  1.3  1.4  1.5  1.6  1.7  1.8  2.0
% 0.005 0.005 0.005 0.005 0.005 0.005 0.005 0.005 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.04 0.04 0.4 0.4
% -> 0.025 0.05 0.075 0.1 0.125 0.15 0.175 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.8 1 3 5
%mydepth = [4, 8, 12, 16, 20]; % cm 10 20 30 40 50
mydepth = [5, 9, 13, 17, 21]; % cm 10 20 30 40 50, first node is atmbc. numerical grid is number zlayer+1

PHI = PHI(mydepth,1);
%mymat = mymat(:,:,mydepth); % added
mymat = mymat(:,mydepth); % added

for i=1:max(size(mydepth))
    mymat(:,i) = mymat(:,i).*PHI(i);
    % % for nodes selected
    % mymat(1,:,i) = mymat(1,:,i).*PHI(i);
    % mymat(2,:,i) = mymat(1,:,i).*PHI(i);
    % mymat(3,:,i) = mymat(1,:,i).*PHI(i);
    % mymat(4,:,i) = mymat(1,:,i).*PHI(i);
end

% S1=interp1(B(:,1),B(:,2),T,'linear','extrap');
% S2=interp1(B(:,1),B(:,3),T,'linear','extrap');
% S3=interp1(B(:,1),B(:,4),T,'linear','extrap');
% S4=interp1(B(:,1),B(:,5),T,'linear','extrap');
% S5=interp1(B(:,1),B(:,6),T,'linear','extrap');


save('mymat.mat','mymat','TIME')

% % plot(mymat(:,mydepth),'color',{'red','blue','green','purple','orange'}, LineWidth=2)
% linecolor = [[1 0 0];[0 0.4470 0.7410];[0.4660 0.6740 0.1880];[0.4940 0.1840 0.5560];[0.8500 0.3250 0.0980]];
% figure
% hold on
% for i =1:size(linecolor,1)
%     plot(TIME(2:end,2)/3600,mymat(2:end,mydepth(i)),'color',linecolor(i,:), LineWidth=2)
% end
% FILENAME = ['../output/wtdepth_point'];
% print('-djpeg','-r300',FILENAME);

clear all
load('tmp.mat')
load('mymat.mat')
delete('tmp.mat')
delete('mymat.mat')
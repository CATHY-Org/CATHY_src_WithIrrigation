% WatertableZ: computes watertable from inputfiles xyz and psi,
%              watertable at depth (d)
% Open the output file dtcoupling
clear all;
% close all;
% surface node to plot
% from 57 to 60 no plants -> no irr no ET
surfnod2plot = [41 42 43 44];

%% import data observed
% date from midnight 21 october (simulation started 24 hours before), EC, H
% EPA SWMM
SWC_meas = readmatrix('R_SWC.csv', 'Delimiter',';');
% SWC_meas(:,1) = linspace(0,86400*2,size(SWC_meas,1));
% SWC_meas(:,3) = SWC_meas(:,3) * 1e-3; % from liter per second to cubic meter per second

%%
addndays = 7; % plot in days
% days added to the simulation to forget IC
dayswarmup = 6;

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
fclose(dtcoupling)
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
fclose(fid)
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

mymat = zeros(size(surfnod2plot,2), nprt,nstr);

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
            VPPSI(d,c)=PSI(b*nnod(1)+d); % .* PHI; % PHI per nnod
        end
    end
    % VPPSI = fliplr(VPPSI);
    for aa=1:size(surfnod2plot,2)
        mymat(aa,a,:) = VPPSI(surfnod2plot(aa),:); % pick a surface node to plot
    end
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
mydepth = [5, 9, 13, 17, 21]; % cm 10 20 30 40 50 % 1 is node for atmbc


% % plot(mymat(:,mydepth),'color',{'red','blue','green','purple','orange'}, LineWidth=2)
%linecolor = [[1 0 0];[0 0.4470 0.7410];[0.4660 0.6740 0.1880];[0.4940 0.1840 0.5560];[0.8500 0.3250 0.0980]];
% plot(mymat(:,mydepth),'color',{'red','ocre','robin egg blue','bright blue','cobalt blue'}, LineWidth=2)
linecolor = [[0.55 0.8 0];[0.8 0.4 0];[0.59 0.87 0.82];[0 0.6000 1];[0 0.28 0.27]];


% figure
% myatmout = 1000.*3600.*ATMDIAGN(:,11);
% myatmout = myatmout / max(abs(myatmout)) / 2;
% stairs(timeatm./86400 - addndays,myatmout,'k');
% hold on
% for i =1:size(linecolor,1)
%     plot(TIME(2:end,2)/3600/24 - addndays,mymat(2:end,mydepth(i)),'color',linecolor(i,:), LineWidth=2);
% end
% 
% FILENAME = ['../output/wtdepth_point'];
% print('-djpeg','-r300',FILENAME);
% close
%%
myscale = [20, 50, 50, 70, 70];
myscale = [100, 100, 100, 100, 100];
xl = 0;
xr = 25;
% figure
% lwd = 1;
% i=0;
% i=i+1;
% subplot(5,1,i)
% hold on
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(1,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(2,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(2,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(3,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(3,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(4,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(4,:), LineWidth=lwd);
% plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color','black', LineWidth=lwd);
% xlim([xl,xr])
% ylim([0,50])
% xlabel('Time (d)')
% ylabel('SWC (%)')
% text(min(xlim), max(ylim), sprintf('Depth: 10 cm'), 'Horiz','left', 'Vert','bottom','FontSize',10,'FontName','Calibri')
% xticks([0:25])
% xtickangle(45)
% yticks([linspace(0,50,6)])
% 
% i=i+1;
% subplot(5,1,i)
% hold on
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(1,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(2,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(2,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(3,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(3,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(4,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(4,:), LineWidth=lwd);
% plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color','black', LineWidth=lwd);
% xlim([xl,xr])
% ylim([0,50])
% xlabel('Time (d)')
% ylabel('SWC (%)')
% text(min(xlim), max(ylim), sprintf('Depth: 20 cm'), 'Horiz','left', 'Vert','bottom','FontSize',10,'FontName','Calibri')
% xticks([0:25])
% xtickangle(45)
% yticks([linspace(0,50,6)])
% 
% i=i+1;
% subplot(5,1,i)
% hold on
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(1,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(2,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(2,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(3,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(3,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(4,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(4,:), LineWidth=lwd);
% plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color','black', LineWidth=lwd);
% xlim([xl,xr])
% ylim([0,50])
% xlabel('Time (d)')
% ylabel('SWC (%)')
% text(min(xlim), max(ylim), sprintf('Depth: 30 cm'), 'Horiz','left', 'Vert','bottom','FontSize',10,'FontName','Calibri')
% xticks([0:25])
% xtickangle(45)
% yticks([linspace(0,50,6)])
% 
% i=i+1;
% subplot(5,1,i)
% hold on
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(1,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(2,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(2,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(3,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(3,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(4,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(4,:), LineWidth=lwd);
% plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color','black', LineWidth=lwd);
% xlim([xl,xr])
% ylim([0,50])
% xlabel('Time (d)')
% ylabel('SWC (%)')
% text(min(xlim), max(ylim), sprintf('Depth: 40 cm'), 'Horiz','left', 'Vert','bottom','FontSize',10,'FontName','Calibri')
% xticks([0:25])
% xtickangle(45)
% yticks([linspace(0,50,6)])
% 
% i=i+1;
% subplot(5,1,i)
% hold on
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(1,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(2,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(2,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(3,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(3,:), LineWidth=lwd);
% plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(4,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(4,:), LineWidth=lwd);
% plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color','black', LineWidth=lwd);
% xlim([xl,xr])
% ylim([0,50])
% xlabel('Time (d)')
% ylabel('SWC (%)')
% text(min(xlim), max(ylim), sprintf('Depth: 50 cm'), 'Horiz','left', 'Vert','bottom','FontSize',10,'FontName','Calibri')
% xticks([0:25])
% xtickangle(45)
% yticks([linspace(0,50,6)])
% 
% FILENAME = ['R_SWC.jpg'];
% print('-djpeg','-r300',FILENAME);




%%
fontsize = 28;
lwd = 3;
figure('units','normalized','outerposition',[0.3 0 0.7 1])
subplot(2,1,1)
hold on
for i=1:5
    plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
end
xlim([xl,xr])
ylim([5,45])
xlabel('Time (d)')
ylabel('SWC (%)')
text(min(xlim), max(ylim), sprintf('Measurements'), 'Horiz','left', 'Vert','bottom','FontSize',fontsize,'FontName','Calibri')
% xticks([0:25])
xticks([linspace(0,24,13)])
%xtickangle(45)
yticks([linspace(5,45,5)])
set(gca,'FontName','Calibri','FontSize',fontsize,'linewidth',2)
%legend({'10 cm','20 cm','30 cm','40 cm','50 cm'}, 'Location', 'north', 'Orientation', 'horizontal', 'FontName','Calibri','FontSize',fontsize,'Box','off','Color','none')
%
subplot(2,1,2)
hold on
for i=1:5
    plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(i,:), LineWidth=lwd);
end
xlim([xl,xr])
ylim([5,45])
xlabel('Time (d)')
ylabel('SWC (%)')
text(min(xlim), max(ylim), sprintf('Simulations'), 'Horiz','left', 'Vert','bottom','FontSize',fontsize,'FontName','Calibri')
% xticks([0:25])
xticks([linspace(0,24,13)])
%xtickangle(45)
yticks([linspace(5,45,5)])
set(gca,'FontName','Calibri','FontSize',fontsize, 'linewidth',2)

FILENAME = ['R_SWC_1fig.png'];
exportgraphics(gcf,FILENAME,'Resolution',300)



%%
fontsize = 28;
lwd = 3;
figure('units','normalized','outerposition',[0.3 0 0.7 1])
h1=axes;
hold on
i=0;
i = i+1;
l1=plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(i,:), LineWidth=lwd);
i = i+1;
l2=plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(i,:), LineWidth=lwd);
i = i+1;
l3=plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(i,:), LineWidth=lwd);
i = i+1;
l4=plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(i,:), LineWidth=lwd);
i = i+1;
l5=plot(TIME(2:end,2)/3600/24 - dayswarmup,mymat(1,2:end,mydepth(i)).*PHI(mydepth(i)-1,1)*myscale(i),'color',linecolor(i,:), LineWidth=lwd);
set(h1,'Xlim',[xl, xr]);
set(h1,'Ylim',[5 50]);
xlabel('Time (d)')
ylabel('SWC (%)')
text(min(xlim), max(ylim), sprintf('Simulations'), 'Horiz','left', 'Vert','bottom','FontSize',fontsize,'FontName','Calibri')
% xticks([0:25])
xticks([linspace(0,24,13)])
yticks([linspace(5,50,10)])
%xtickangle(45)
set(gca,'FontName','Calibri','FontSize',fontsize,'linewidth',2)
%legend({'10 cm','20 cm','30 cm','40 cm','50 cm'}, 'Location', 'north', 'Orientation', 'horizontal', 'FontName','Calibri','FontSize',fontsize,'Box','off','Color','none')
%
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
%
h2=axes;
% begin with 0 time and irrigation and end with midnight
timeatm_h = [0 16 10 9.66 10.66 11 8.66 13.66 13.5 9.5 23.99];
timeatm_d = [0 0  1  2    3     4  6    8     14   20  24];
timeatm = timeatm_d + timeatm_h/24;
y2 =        [0 20 20 10.1 5     5  9.8  5     10   5   0];
y0 = zeros(size(y2));

timeatm3 = [timeatm' timeatm' [timeatm+1/24]']';
timeatm3 = timeatm3(:);
y3 = [y0' y2' y0']';
y3 = y3(:);
l6=line(timeatm3,y3,'color','black', 'LineWidth',lwd);
set(h2,'YDir','reverse');
set(h2,'Xlim',[xl, xr]);
set(h2,'Ylim',[0 45]);
xticks([linspace(0,24,13)])
yticks([linspace(0,45,10)])
%xtickangle(45)
set(h2,'YAxisLocation','Right');
set(h2, 'Color', 'None');
set(h2, 'Xtick', []);
ylabel('Irrigation (mm h^{-1})');

legend( [l1;l2;l3;l4;l5;l6] , '10','20','30','40','50','Spr','Location','south', 'Orientation', 'horizontal','Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['R_SWC_1fig_atm_sim.png'];
exportgraphics(gcf,FILENAME,'Resolution',300)




jfind = find(SWC_meas(:,10)/3600/24  - addndays >= 24, 1);
figure('units','normalized','outerposition',[0.3 0 0.7 1])
h1=axes;
hold on
i=0;
i = i+1;
%l1=plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
l1=plot(SWC_meas(1:jfind,10)/3600/24 - addndays,SWC_meas(1:jfind,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
i = i+1;
%l2=plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
l2=plot(SWC_meas(1:jfind,10)/3600/24 - addndays,SWC_meas(1:jfind,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
i = i+1;
%l3=plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
l3=plot(SWC_meas(1:jfind,10)/3600/24 - addndays,SWC_meas(1:jfind,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
i = i+1;
%l4=plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
l4=plot(SWC_meas(1:jfind,10)/3600/24 - addndays,SWC_meas(1:jfind,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
i = i+1;
%l5=plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
l5=plot(SWC_meas(1:jfind,10)/3600/24 - addndays,SWC_meas(1:jfind,i+4),'-','color',linecolor(i,:), LineWidth=lwd+0.5);
set(h1,'Xlim',[xl, xr]);
set(h1,'Ylim',[5 50]);
xlabel('Time (d)')
ylabel('SWC (%)')
text(min(xlim), max(ylim), sprintf('Measurements'), 'Horiz','left', 'Vert','bottom','FontSize',fontsize,'FontName','Calibri')
% xticks([0:25])
xticks([linspace(0,24,13)])
%xtickangle(45)
yticks([linspace(5,50,10)])
set(gca,'FontName','Calibri','FontSize',fontsize,'linewidth',2)
%legend({'10 cm','20 cm','30 cm','40 cm','50 cm'}, 'Location', 'north', 'Orientation', 'horizontal', 'FontName','Calibri','FontSize',fontsize,'Box','off','Color','none')
%
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
%
h2=axes;
% begin with 0 time and irrigation and end with midnight
timeatm_h = [0 16 10 9.66 10.66 11 8.66 13.66 13.5 9.5 23.99];
timeatm_d = [0 0  1  2    3     4  6    8     14   20  24];
timeatm = timeatm_d + timeatm_h/24;
y2 =        [0 20 20 10.1 5     5  9.8  5     10   5   0];
y0 = zeros(size(y2));

timeatm3 = [timeatm' timeatm' [timeatm+1/24]']';
timeatm3 = timeatm3(:);
y3 = [y0' y2' y0']';
y3 = y3(:);
l6=line(timeatm3,y3,'color','black', 'LineWidth',lwd);
set(h2,'YDir','reverse');
set(h2,'Xlim',[xl, xr]);
set(h2,'Ylim',[0 45]);
set(h2,'YAxisLocation','Right');
set(h2, 'Color', 'None');
set(h2, 'Xtick', []);
ylabel('Irrigation (mm h^{-1})');

legend( [l1;l2;l3;l4;l5;l6] , '10','20','30','40','50','Spr','Location','south', 'Orientation', 'horizontal','Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['R_SWC_1fig_atm_mes.png'];
exportgraphics(gcf,FILENAME,'Resolution',300)

return
%%

figure
subplot(2,1,1)
myatmout = 1000.*3600.*ATMDIAGN(:,11);
myatmout = myatmout / max(abs(myatmout)) / 2;
stairs(timeatm./86400 - addndays,myatmout,'k');
hold on
for i =1:size(linecolor,1)
    plot(TIME(2:end,2)/3600/24 - addndays,mymat(2:end,mydepth(i)),'color',linecolor(i,:), LineWidth=2);
end

xlim([0, 15])
%ylim([0, 0.4])

subplot(2,1,2)
stairs(timeatm./86400 - addndays,myatmout,'k');
hold on
for i =1:size(linecolor,1)
    % plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,4+i)/max(max(SWC_meas(:,5:9))),'color',linecolor(i,:), LineWidth=2, Linestyle='--');
    plot(SWC_meas(:,10)/3600/24 - addndays,SWC_meas(:,4+i),'color',linecolor(i,:), LineWidth=2, Linestyle='-');
end

xlim([0, 15])

FILENAME = ['../output/wtdepth_point_compare'];
print('-djpeg','-r300',FILENAME);


return
%% movie
% do interp with field data
S1=interp1(SWC_meas(:,10),SWC_meas(:,5)/100*2, TIME(:,2),'linear','extrap'); % sample point, sample values, query points at 10 cm
S2=interp1(SWC_meas(:,10),SWC_meas(:,6)/100*2, TIME(:,2),'linear','extrap'); % sample point, sample values, query points at 20 cm
S3=interp1(SWC_meas(:,10),SWC_meas(:,7)/100*2, TIME(:,2),'linear','extrap'); % sample point, sample values, query points at 30 cm
S4=interp1(SWC_meas(:,10),SWC_meas(:,8)/100*2, TIME(:,2),'linear','extrap'); % sample point, sample values, query points at 40 cm
S5=interp1(SWC_meas(:,10),SWC_meas(:,9)/100*2, TIME(:,2),'linear','extrap'); % sample point, sample values, query points at 50 cm
mydeep = [0, .05, linspace(.1,1.8,18), 2.0];

figure
for i =1:size(mymat,1)
    drawnow
    % plot(mymat(i,:), mydeep, "-k")
    plot(mymat(i,:), mydeep, "-k", S1(i), mydeep(2), "o", S2(i), mydeep(3), "o", S3(i), mydeep(4), "o", S4(i), mydeep(5), "o", S5(i), mydeep(6), "o")
    set(gca, 'YDir','reverse');
    xlim([0, 1])
    xlabel('SWC (-)')
    xlabel('Soil Depth (m)')
    text(0.05,0.2, ['time (d) ', num2str(TIME(i,2)/3600/24)]);
    pause(0.2)
end

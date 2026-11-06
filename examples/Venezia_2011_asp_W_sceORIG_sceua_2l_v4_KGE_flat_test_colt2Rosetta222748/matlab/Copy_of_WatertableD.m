% WatertableZ: computes watertable from inputfiles xyz and psi,
%              watertable at depth (d)
% Open the output file dtcoupling
clear all;
close all;
delete WatertableD.m~;
%bores=load('/Users/campo/Work/ICEA/tesi/dottorato/gatto/Cathy/output/pozzi.dat');
dtcoupling = fopen('/Users/campo/Work/ICEA/tesi/dottorato/gatto/Cathy/output/dtcoupling','r');
%Read in the file
a=0;
nstep=0;
ATMDIAGN=[];
while (a~=(-1))              % Find out number of lines and timesteps (nstep)
    a = fgetl(dtcoupling);   % Loops trough dtcoupling until a=-1 end of file
    nstep = nstep+1;
end
a=0;
nstep = nstep-31;
%nstep = nstep-(26+1); % Number of timesteps
frewind(dtcoupling);
%for i = 1:24
 for i = 1:28
    fgets(dtcoupling);
end
ATMDIAGN = fscanf(dtcoupling,'%g',[22,nstep]); % Read data
ATMDIAGN = ATMDIAGN';
close ('all');
jmax=max(ATMDIAGN(:,11));
timeatm(1)=ATMDIAGN(1,3);
for i = 2:nstep
    timeatm(i)=timeatm(i-1)+ATMDIAGN(i-1,2);
end

xyz = fopen('/Users/campo/Work/ICEA/tesi/dottorato/gatto/Cathy/output/xyz','r');
psi = fopen('/Users/campo/Work/ICEA/tesi/dottorato/gatto/Cathy/output/psi','r');

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

for a=1:nprt;
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
    VPPSI = fliplr(VPPSI);
    % Start from the bottom of the profile, and proceed upwards
    % until node with negative pressure head is encountered.
    % Calculate watertable between this node and the one directly below
    % by linear interpolation.
    for b=1:nnod(1)
        for c=1:nstr-1
            if VPPSI(b,c)>=0 & VPPSI(b,c+1)<0 & FLAG(b,a)==0
                % Watertable, interpolate linearly (Z=rc*VPPSI+z0)
                rc = (Z(b,c)-Z(b,c+1))/(VPPSI(b,c)-VPPSI(b,c+1));
                Z0(b) = Z(b,c)-rc*VPPSI(b,c);
                FLAG(b,a)=1;
            elseif VPPSI(b,c)>=0 & VPPSI(b,c+1)<0 & FLAG(b,a)==1
                % Second watertable
                FLAG(b,a)=2;
            elseif c==nstr-1 & VPPSI(b,c+1)>=0 & FLAG(b,a)==0
                % Watertable not encounterd and nodes stil saturated
                FLAG(b,a)=3;
            elseif c==nstr-1 & FLAG(b,a)==0
                % Watertable not encounterd, and not fully saturated
                % profile. Set watertable to lowest node
                Z0(b) = Z(b,c-nstr+2);
                FLAG(b,a)=4;
            end
        end
    end
    Z0 = XYZsurface(:,4)-Z0;
%     l3=[];
%     for i=1:n1;
%         l3=[l3;Z0(L3NOD(i))];% Z0A(L3NOD(i))];    %Water table only for the selected nodes
%     end
%     scr3=[];
%     for i=1:n1/4;
%         scr3=[scr3;mean(l3((i-1)*4+1:i*4))];
%     end
%     L3=[L3;scr3'];
%     L3=[L3;mean(l3)];
%     l11=[];
%     for i=1:n2;
%         l11=[l11;Z0(L11NOD(i))];% Z0A(L3NOD(i))];    %Water table only for the selected nodes
%     end
%     scr11=[];
%     for i=1:n2/4;
%         scr11=[scr11;mean(l11((i-1)*4+1:i*4))];
%     end
%     L11=[L11;scr11'];
%     L11=[L11;mean(l11)];
    %l3=l3';
    %L3=[L3;l3];
%     l3=[];
%     %l11=[];
%     for i=1:n
%         l3=[l3;Z0(L3NOD(i))];
%         %l11=[l11;Z0(L11NOD(i))];
%     end
%     l3=l3';
%     %l11=l11';
%     L3=[L3;l3];
    %L11=[L11;l11];
    if a==1
        % Move area to (0,0)
        XYZsurface(:,2) = XYZsurface(:,2) - minx;
        XYZsurface(:,3) = XYZsurface(:,3) - miny;
        % Determine grid -> size of Z00
        length = XYZsurface(1,3)-XYZsurface(2,3);
        width = XYZsurface(1,2)-XYZsurface(2,2);
        grid = abs(length - width);
        % Turn (x,y)values into indices for Z00
        XYZsurface(:,2) = XYZsurface(:,2)/grid+1;
        XYZsurface(:,3) = XYZsurface(:,3)/grid+1;
        %Create X and Y
        X = [(minx-grid):grid:(maxx+grid)];
        Y = [(miny-grid):grid:(maxy+grid)];
    end
    % Create Z00, not with reshape,
    % you don't know the shape of the catchment
    Z00 = zeros(round((maxy-miny)/grid+1),round((maxx-minx)/grid+1));
    Z00(:,:) = NaN;
    for b=1:nnod(1)
        Z00(round(XYZsurface(b,3)),round(XYZsurface(b,2))) = Z0(b);
    end
    SizeZ00 = size(Z00);
    %     % Add a line of NaNs around the catchment
    %     Z00ZEROS = zeros(1,SizeZ00(2));
    %     Z00ZEROS(:,:) = NaN;
    %     Z00 = [Z00ZEROS;Z00;Z00ZEROS];
    %     Z00 = Z00';
    %     SizeZ00 = size(Z00);
    %     Z00ZEROS = zeros(1,SizeZ00(2));
    %     Z00ZEROS(:,:) = NaN;
    %     Z00 = [Z00ZEROS;Z00;Z00ZEROS];
    %     Z00 = Z00';
    %     SizeZ00 = size(Z00);
    %    [i,j] = find(Z00==0); % Turn zeros into NaN
    %    Sizei = size(i);
    %    for b=1:Sizei(1)
    %        Z00(i(b),j(b))=NaN;
    %    end
    if a==1 % Create matrix with 1 in catchment and 0 outside the catchment
        CATCHMENT = isfinite(Z00);
        [y,x]=contouring(CATCHMENT,1);
        x = (grid*x)+(minx-2*grid);
        y = (grid*y)+(miny-2*grid);
    end
    gridsize=(maxx-minx)/(SizeZ00(2)-1);
    X = [(minx):gridsize:(maxx)];
    gridsize=(maxy-miny)/(SizeZ00(1)-1);
    Y = [(miny):gridsize:(maxy)];
    % Create contourlevels
    maxz00=max(max(Z00));
    minz00=min(min(Z00));
    CONTOURLEVELS = [minz00:(maxz00-minz00)/3:maxz00];
    %   CONTOURLEVELS = round(CONTOURLEVELS);   % To get contourlevels at integer values
    if isempty(CONTOURLEVELS);
        CONTOURLEVELS = [0 0 0 0];
    end
    % Force zlim the same for every timestep
    if maxz00all < maxz00
        maxz00all = maxz00;
    end
    if minz00all > minz00
        minz00all = minz00;
    end

    % Plot graph
    figure(a);
    set(gcf,'Renderer','zbuffer');
    %%%   Figure as contourplot
    %%    hold on
    %%    plot(x,y,'k-')
    %%    contour(X,Y,Z00,[(CONTOURLEVELS(1)) (CONTOURLEVELS(1))],'-k')
    %%    contour(X,Y,Z00,[(CONTOURLEVELS(2)) (CONTOURLEVELS(2))],':k')
    %%    contour(X,Y,Z00,[(CONTOURLEVELS(3)) (CONTOURLEVELS(3))],'--k')
    %%    contour(X,Y,Z00,[(CONTOURLEVELS(4)) (CONTOURLEVELS(4))],'-.k')
    %%    axis([(minx-gridsize) (maxx+gridsize) (miny-gridsize) (maxy+gridsize)])
    %%    title(['Watertable (m) at ',num2str(TIME(a,2)),' h'])
    %%    xlabel('(m)')
    %%    ylabel('(m)')
    %%    % Add legend
    %%    axes('position',[0 0 1 1])
    %%    axis([0 1 0 1]), axis off
    %%    k = line([0.70 0.76],[0.88 0.88]);
    %%    set(k,'linestyle','-','color','k')
    %%    text(0.79,0.88,num2str(CONTOURLEVELS(1),3)), text(0.85,0.88,'m')
    %%    l = line([0.70 0.76],[0.85 0.85]);
    %%    set(l,'linestyle',':','color','k')
    %%    text(0.79,0.85,num2str(CONTOURLEVELS(2),3)), text(0.85,0.85,'m')
    %%    m = line([0.70 0.76],[0.82 0.82]);
    %%    set(m,'linestyle','--','color','k')
    %%    text(0.79,0.82,num2str(CONTOURLEVELS(3),3)), text(0.85,0.82,'m')
    %%    n = line([0.70 0.76],[0.79 0.79]);
    %%    set(n,'linestyle','-.','color','k')
    %%    text(0.79,0.79,num2str(CONTOURLEVELS(4),3)), text(0.85,0.79,'m')

    %%% Figure as shaded surface
    h(1) = axes('Position',[0.1 0.8 0.8 .1]);
    stairs(timeatm./86400,1000.*3600.*ATMDIAGN(:,11),'k');
    hold on
    plot(sbarr(:,1)./86400,sbarr(:,2),'r+','MarkerSize',10);
    set(h(1),...
        'XAxisLocation','top');
    %    'YLim',[0 ceil(jmax*1000/100)*100],...%'YAxisLocation','right',...
    %    'YDir','reverse');
    ylabel('Atm. forcing (mm/h)');
    xlabel('days');
    h(2) = axes('Position',[0.1 0.1 0.8 0.6]);
    %plot(x,y,'k-')
    hold on
    axis equal
    surf(X,Y,Z00)
    %plot3(bores(:,2),bores(:,3),bores(:,4),'k.');
    %grid off
    shading flat
    colormap jet%gray
    %title(['Water table distribution (m below surface) at ',num2str(floor(TIME(a,2))),' d'])
    xlabel('Easting (m)')
    ylabel('Northing (m)')
    set(gca,'Color','w');
    %hold off
    % Create postscript outputfile
    FILENAME = ['/Users/campo/Work/ICEA/tesi/dottorato/gatto/Cathy/output/wtd',num2str(round(TIME(a,2)),'%04d')];
    %print('-deps','-r200',FILENAME);
    saveas(gcf,FILENAME,'fig');
    close all
    Z00=[];
end
for a=1:nprt;
    FILENAME = ['/Users/campo/Work/ICEA/tesi/dottorato/gatto/Cathy/output/wtd',num2str(round(TIME(a,2)),'%04d')];
    openfig(FILENAME);
    caxis([minz00all maxz00all])
    colorbar
    saveas(gcf,FILENAME,'fig');
    FILENAME = ['/Users/campo/Work/ICEA/tesi/dottorato/gatto/Cathy/output/wtd',num2str(round(TIME(a,2)),'%04d')];
    print('-djpeg','-r300',FILENAME);
    close all
    %FRAMEWTD(a)=getframe(gcf);
    %close all
end
FLAG(:,nprt+1) = XYZsurface(:,1);
fclose(psi);
%FALDA=[TIME(:,2)-1512 L3(:,1:9)];
%FALDA=[TIME(:,2) L3(:,1:9)];
% FALDA=[TIME(:,2) L3];
% save '/Users/campo/Work/papers/journals/cathy-LCC/runs/2009/virtual-transect/homog-flat/2009/p2/wtdL3.dat' -ASCII FALDA
%FALDA=[TIME(:,2)-1512 L3(:,10:18)];
%FALDA=[TIME(:,2) L3(:,10:18)];
% FALDA=[TIME(:,2) L11];
% save '/Users/campo/Work/papers/journals/cathy-LCC/runs/2009/virtual-transect/homog-flat/2009/p2/wtdL11.dat' -ASCII FALDA
%save '/Users/campo/Work/papers/journals/cathy-LCC/runs/2009/virtual-transect/homog-flat/2009/p2/falda.dat' -ASCII FALDA
%save '/Users/campo/Work/papers/journals/cathy-LCC/runs/2009/virtual-transect/homog-flat/2009/p2/L11.dat' -ASCII L11
%movie2avi(FRAMEWTD,'E:\swimer\cathy\avi\wtd.avi','fps',3,'compression','Indeo5');

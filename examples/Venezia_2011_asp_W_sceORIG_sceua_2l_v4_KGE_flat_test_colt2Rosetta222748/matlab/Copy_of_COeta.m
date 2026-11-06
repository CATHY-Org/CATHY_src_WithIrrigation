%COeta: Processes actual evapotranspiration and creates graphs
%Open the output file dtcoupling
close all;
clear all;
delete COeta.m~;
scrsz = get(0,'ScreenSize');
dtcoupling = fopen('/Users/campo/Work/papers/journals/paired-catchments/PSIMINvsRWU/M1/dry/rwu/OXSTRESS/run3pmin/dtcoupling','r');
%Read in the file
a=0;
nstep=0;
ATMDIAGN=[];
while (a~=(-1))              % Find out number of lines and timesteps (nstep)
    a = fgetl(dtcoupling);   % Loops trough dtcoupling until a=-1 end of file
    nstep = nstep+1;
end
a=0;
nstep = nstep-31;%(26+1); % Number of timesteps
frewind(dtcoupling);
for i = 1:28%24
    fgets(dtcoupling);
end
ATMDIAGN = fscanf(dtcoupling,'%g',[22,nstep]); % Read data
ATMDIAGN = ATMDIAGN';
% ATMDIAGN = load('/Users/campo/Work/papers/journals/paired-catchments/PSIMINvsRWU/M1/dry/rwu/OXSTRESS/run3pmin/dtcoupling');
close ('all');
jmax=max(ATMDIAGN(:,11));
time(1)=ATMDIAGN(1,3);
nstep = size(ATMDIAGN,1);
for i = 2:nstep
    time(i)=time(i-1)+ATMDIAGN(i-1,2);
end

psisurf = fopen('/Users/campo/Work/papers/journals/paired-catchments/PSIMINvsRWU/M1/dry/rwu/OXSTRESS/run3pmin/fort.777','r');

% Read in the file
a=0;
b=0;
c=0;
nprt=0;
nnod=0;
linesinpsisurf=0;
TIMEPRT=[];
PSISURF=[];
while (a~=(-1))          % Find out number of time values for detailed nodal output (nprt)
    a = fgetl(psisurf);  % and number of surface nodes (nnod)
    linesinpsisurf = linesinpsisurf+1; % Loops trough psisurf until a=-1 end of file
    if (a~=(-1))
        b = double(a);
    end
    if b(1:5)==[32 83 85 82 70] % When a(1:5)=' SURF' starts a new timestep
        nprt = nprt+1;          % a(1:5)=' SURF' -> b(1:5)=[32 83 85 82 70]
    end
end
linesinpsisurf = linesinpsisurf-1;
nnod = (linesinpsisurf-(2*nprt))/nprt;
frewind(psisurf);
for c=1:nprt % Read data
    timeprt = fscanf(psisurf,'%u %g',[2,1]);
    timeprt = timeprt';
    TIMEPRT = [TIMEPRT;timeprt]; % TIMEPRT contains timestep and times of nprt
    fgets(psisurf);
    fgets(psisurf);
    PsiSurf = fscanf(psisurf,'%u %g %g %g',[4,nnod]);
    PsiSurf = PsiSurf';
    PSISURF = [PSISURF;PsiSurf];
end
close('all');

% Plot the graph
% Use only square gridcells
a=0;
b=0;
length=0;
width=0;
gridsize=0;
maxx=0;
maxy=0;
minx=0;
miny=0;
minpsi=NaN;
minpsiall=inf;
maxpsi=NaN;
maxpsiall=-inf;
saturated = 0;
minPSI=0;
maxPSI=0;
X=[];
Y=[];
PSI=[];
SizePSI=[];
CATCHMENT=[];
PSIZEROS=[];
CONTOURLEVELS=[];
FILENAME=[];

maxx=max(PSISURF(:,2));
maxy=max(PSISURF(:,3));
maxpsi=max(PSISURF(:,4));
minx=min(PSISURF(:,2));
miny=min(PSISURF(:,3));
minpsi=min(PSISURF(:,4));
saturated = max(PSISURF(1:nnod,4));

% Move area to (0,0)
PSISURF(:,2) = PSISURF(:,2) - minx;
PSISURF(:,3) = PSISURF(:,3) - miny;
% Determine gridsize -> size of PSI
length = PSISURF(1,3)-PSISURF(2,3);
width = PSISURF(1,2)-PSISURF(2,2);
gridsize = abs(length - width);
% Turn (x,y) values into indices for PSI
PSISURF(:,2) = PSISURF(:,2)/gridsize+1;
PSISURF(:,3) = PSISURF(:,3)/gridsize+1;
for a=1:nprt
    % Create PSI, not with reshape, you don't know the shape of the catchment
    PSI=[];
    PSI = zeros(round((maxy-miny)/gridsize+1),round((maxx-minx)/gridsize+1));
    PSI(:,:) = NaN;
    sbarr=[TIMEPRT(a,2) 0];
    for b=1:nnod
        I1 = round(PSISURF(b+(a-1)*nnod,3));
        I2 = round(PSISURF(b+(a-1)*nnod,2));
        PSI(I1,I2) = PSISURF(b+(a-1)*nnod,4);
    end
    SizePSI = size(PSI);
    %     PSIZEROS = zeros(1,SizePSI(2)); % Add a ring of NaNs around the catchment
    %     PSIZEROS(:,:) = NaN;
    %     PSI = [PSIZEROS;PSI;PSIZEROS];
    %     PSI = PSI';
    %     SizePSI = size(PSI);
    %     PSIZEROS = zeros(1,SizePSI(2));
    %     PSIZEROS(:,:) = NaN;
    %     PSI = [PSIZEROS;PSI;PSIZEROS];
    %     PSI = PSI';
    %     SizePSI = size(PSI);
    if a==1 % Create matrix with 1 in catchment and 0 outside the catchment
        MEANPSI=zeros(size(PSI));
        CATCHMENT = isfinite(PSI);
        [y,x]=contouring(CATCHMENT,1);
        x1 = minx + (x - min(x)).*(maxx-minx)./(max(x) - min(x));
        y1 = miny + (y - min(y)).*(maxy-miny)./(max(y) - min(y));
        %         x1 = (x-0.0)*gridsize+minx-2*gridsize;
        %         y1 = (y-0.0)*gridsize+miny-2*gridsize;
        gridsize=(maxx-minx)/(SizePSI(2)-1);
        X = [(minx):gridsize:(maxx)];
        gridsize=(maxy-miny)/(SizePSI(1)-1);
        Y = [(miny):gridsize:(maxy)];
    end
    %    [i,j] = find(PSI==0); % Turn zeros into NaN
    %    Sizei = size(i);
    %    for b=1:Sizei(1)
    %        PSI(i(b),j(b))=NaN;
    %    end
    % Create contourlevels
    %     minPSI = min(min(PSI));
    %     maxPSI = max(max(PSI));
    %     if maxpsiall < maxpsi
    %         maxpsiall = maxpsi;
    %     end
    %     if minpsiall > minpsi
    %         minpsiall = minpsi;
    %     end
    %     CONTOURLEVELS = [minPSI:(maxPSI-minPSI)/3:maxPSI];
    %     %    CONTOURLEVELS = round(CONTOURLEVELS); % To get contourlevels as integer values
    %     if isempty(CONTOURLEVELS);
    %         CONTOURLEVELS = [0 0 0 0];
    %     end
    %Plot graph
    f(a)=figure('Position',[1 scrsz(4) scrsz(3)/2 scrsz(4)]);    %%% Figure as contourplot
    %%hold on
    %%plot(x,y,'k-')
    %%contour(X,Y,PSI,[(CONTOURLEVELS(1)) (CONTOURLEVELS(1))],'-k')
    %%contour(X,Y,PSI,[(CONTOURLEVELS(2)) (CONTOURLEVELS(2))],':k')
    %%contour(X,Y,PSI,[(CONTOURLEVELS(3)) (CONTOURLEVELS(3))],'--k')
    %%contour(X,Y,PSI,[(CONTOURLEVELS(4)) (CONTOURLEVELS(4))],'-.k')
    %%axis([(minx-gridsize) (maxx+gridsize) (miny-gridsize) (maxy+gridsize)])
    %%title(['Pressure head at surface nodes at ',num2str(TIMEPRT(a,2)),' h'])
    %%xlabel('(m)')
    %%ylabel('(m)')
    %%axes('position',[0 0 1 1])  % add legend
    %%axis([0 1 0 1]), axis off
    %%k = line([0.70 0.76],[0.88 0.88]);
    %%set(k,'linestyle','-','color','k')
    %%text(0.79,0.88,num2str(CONTOURLEVELS(1),3)), text(0.85,0.88,'    m')
    %%l = line([0.70 0.76],[0.85 0.85]);
    %%set(l,'linestyle',':','color','k')
    %%text(0.79,0.85,num2str(CONTOURLEVELS(2),3)), text(0.85,0.85,'    m')
    %%m = line([0.70 0.76],[0.82 0.82]);
    %%set(m,'linestyle','--','color','k')
    %%text(0.79,0.82,num2str(CONTOURLEVELS(3),3)), text(0.85,0.82,'    m')
    %%n = line([0.70 0.76],[0.79 0.79]);
    %%set(n,'linestyle','-.','color','k')
    %%text(0.79,0.79,num2str(CONTOURLEVELS(4),3)), text(0.85,0.79,'    m')
    
    %%% Figure as shaded surface
    %     h(1) = axes('Position',[0.1 0.8 0.8 0.1]);
    %     stairs(time./86400,1000*86400*ATMDIAGN(:,11),'k');
    %     hold on
    %     plot(sbarr(:,1)./86400,sbarr(:,2),'+r','MarkerSize',18,'LineWidth',2);
    %     %set(h(1),...
    %     %    'XAxisLocation','top',...
    %     %    'YLim',[0 ceil(jmax*1000/100)*100],...%'YAxisLocation','left',...
    %     %    'YDir','reverse');
    %     ylabel('pot. atm. flux (mm/d)','FontSize',18);
    %     xlabel('days','FontSize',18);
    %     xlim([0 350]);
    %     set(gca,'FontSize',18,'XAxisLocation','top');
    %     h(2) = axes('Position',[0.1 0.1 0.8 0.6]);
    %     imagesc(X,Y,1000*86400*PSI./400);
    %     set(gca,'YDir','normal');
    surf(X,Y,1000*86400*PSI,'LineStyle',':','LineWidth',0.2);
    MEANPSI=MEANPSI+1000*86400*PSI;
    view([0 90]);
    grid off
    %shading flat
    colormap(1-gray)%(flipdim(jet,1))
    %title(['Pressure head at surface nodes at ',num2str(round(TIMEPRT(a,2))),' d'])
    %set(h(2),...
    %    'XAxisLocation','top');
    xlabel('Easting (m)','FontSize',18)
    ylabel('Northing (m)','FontSize',18)
    caxis ([0 max(max(1000*86400*PSI))]);
    %caxis ([-10.0 5.0]);
    colorbar
    set(gcf,'Renderer','Zbuffer');
    axis equal
    xlim([minx maxx])
    %xlim([357000 365000])
    ylim([miny maxy])
    %     hold on
    %     plot(x1,y1,'k-')
    set(gca,'FontSize',18);
    %axis equal
    %xlim([0 31]);
    %     FILENAME = ['E:\MJ\output_62\graphrecharge',num2str(round(TIMEPRT(a,2)),'%04d')];
    %     saveas(gcf,FILENAME,'fig');
    %     set(gcf,'PaperPositionMode','auto')
    FILENAME = ['/Users/campo/Work/papers/journals/paired-catchments/PSIMINvsRWU/M1/dry/rwu/OXSTRESS/run3pmin/eta',num2str(round(TIMEPRT(a,2)),'%04d')];
    print('-djpeg','-r300',FILENAME);
    %     hold off
    close all
end
MEANPSI=MEANPSI./nprt;
surf(X,Y,MEANPSI,'LineStyle',':','LineWidth',0.2);
view([0 90]);
grid off
colormap(1-gray)
xlabel('Easting (m)','FontSize',18)
ylabel('Northing (m)','FontSize',18)
caxis([0 max(max(MEANPSI))]);
colorbar
set(gcf,'Renderer','Zbuffer');
axis equal
xlim([minx maxx])
ylim([miny maxy])
set(gca,'FontSize',18);
FILENAME = ['/Users/campo/Work/papers/journals/paired-catchments/PSIMINvsRWU/M1/dry/rwu/OXSTRESS/run3pmin/etamean'];
print('-djpeg','-r300',FILENAME);

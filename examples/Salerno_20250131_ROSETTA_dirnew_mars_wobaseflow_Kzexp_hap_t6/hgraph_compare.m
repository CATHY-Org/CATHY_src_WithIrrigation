clear all

% remove wrong data measured
myst = 17.3646;
myen = 21.3472;

% parameters
warmupCATHY = 183; % days. for integration volumes
parmTMAX = 6304200/86400; % days- for calculating integrals
offsetsensor = -0/24; % days. this is because rainfall may take a while from battipaglia to the catchment. it is based on matching the water level rise

%%
% get current foldername
[~,foldername,~]=fileparts(pwd);

%% ATMBC
dtcoupling = fopen('output/dtcoupling','r');
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

timeatm = timeatm/86400 - warmupCATHY;
myatmout = 1000.*3600.*ATMDIAGN(:,11);
myatmout = myatmout / max(abs(myatmout)) / 2;

%% normal hgraph script
HG=importdata('output/hgraph',' ',2);

Tsim=HG.data(:,1)./86400 - warmupCATHY; % days
%Tsim1_pos=datenum(2023,12,04,23,59,0)+HG1.data(:,1)./86400;
[C,IA,IC] = unique(Tsim);

Qsim=HG.data(IA,2); % could be second or third column
if (sum(Qsim)>0)
    fprintf('reading column 2\n')
else
    fprintf('reading column 3\n')
    Qsim=HG.data(IA,3); % could be second or third column
end

% save one
Tsim1 = Tsim(IA);
Qsim1 = Qsim;
clearvars C IA IC HG
% Tsim_pos = Tsim_pos(IA);
% Qsim1=interp1(Tsim(IA),HG1.data(IA,2),T1);
% [E2,D2,E1,D1,RMSE,NRMSE,KGE]=efficiency(Qsim',Q1)

% figure
% movegui('west');
% plot(Tsim1,Qsim1,'LineWidth',1.5);
% % hold on
% % stairs(timeatm./86400,myatmout,'k');
% % dateaxis('X',2)
% set(gca,'FontSize',14)%,'YScale','log')
% xtickangle(20)
% ylabel('Streamflow (m^3/s)');
% legend('Discharge','Rain','Location','NorthWest')
% % FILENAME = ['../output/hgraph'];
% % print('-djpeg','-r300',FILENAME);
% % close


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HG=importdata(['../' foldername '_open_phi_K/output/hgraph'],' ',2);
% HG=importdata(['../' foldername '_open_KC1/output/hgraph'],' ',2);
HG=importdata(['../Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzexp_hap_t6_VEG/output/hgraph'],' ',2);

Tsim=HG.data(:,1)./86400 - warmupCATHY; % days
%Tsim_pos=datenum(2023,6,13,0,0,0)+HG.data(:,1)./86400;
[C,IA,IC] = unique(Tsim);

Qsim=HG.data(IA,2); % could be second or third column
if (sum(Qsim)>0)
    fprintf('reading column 2\n')
else
    fprintf('reading column 3\n')
    Qsim=HG.data(IA,3); % could be second or third column
end

Tsim = Tsim(IA);

Tsim2 = Tsim;
%Tsim_pos2 = Tsim_pos(IA);
Qsim2 = Qsim;
clearvars C IA IC HG

%%
% day -- hour -
% normal 2soils = red
% normal 1soil = green
% moverain 2soils = blue
% moverain 1soil = cyan

% ic 2 m is black
figure()
hold on
plot(Tsim1,Qsim1,'-','LineWidth',2.5,'Color',[0.25, 0.25, 0.25]); % with greenhouse
plot(Tsim2,Qsim2,'-','LineWidth',1.5,'Color',[0.9290, 0.6940, 0.1250]); % only open field
% dateaxis('X',2)
set(gca,'FontSize',14)%,'YScale','log')
legend('w Greenhouse','wo Greenhouse','Location','NorthWest')
xtickangle(20)
ylabel('Streamflow (m^3/s)');
FILENAME = ['hgraph_compare'];
print('-djpeg','-r300',FILENAME);









%%
lwd = 4;
figure('units','normalized','outerposition',[0.3 0 0.7 1])
h1=axes;
hold on
l1=line(Tsim1,Qsim1,'LineWidth',lwd,'color',[0.25, 0.25, 0.25]);
l2=line(Tsim2,Qsim2,'LineWidth',lwd-1,'color',[0.9290, 0.6940, 0.1250]);
xlabel('Time (d)');
ylabel('Streamflow (m³/s)')
set(h1,'Xlim',[0 ceil(max(timeatm))+10]);
set(h1,'Ylim',[0 40]);
grid on
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
%
h2=axes;
y2 = 1000.*3600.*ATMDIAGN(:,11);
l3=line(timeatm,y2,'color','blue', 'LineWidth',lwd-2);
h2.Position = h1.Position;
set(h2,'YDir','reverse');
set(h2,'Xlim',[0 ceil(max(timeatm))+10]);
set(h2,'Ylim',[0 40]);
set(h2,'YAxisLocation','Right');
set(h2, 'Color', 'None');
set(h2, 'Xtick', []);
ylabel('Atmbc (mm/h)');

legend( [l1;l2;l3] , 'w greenhouse','w/o greenhouse','Atmbc','Location','west','Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')

FILENAME = ['hgraph_compare_atm'];
print('-djpeg','-r300',FILENAME);


%%
figure('units','normalized','outerposition',[0.3 0 0.7 1])
set(gcf,'Renderer','zbuffer');

%%% Figure as shaded surface
h(1) = axes('Position',[0.1 0.8 0.8 .1]);
y2 = 1000.*3600.*ATMDIAGN(:,11);
stairs(timeatm,y2,'k');
hold on
%plot(sbarr(:,1)./86400,sbarr(:,2),'r+','MarkerSize',10);
set(h(1),...
    'XAxisLocation','top');
ylabel('Atmbc (mm/h)');
%xlabel('days');
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
h(2) = axes('Position',[0.1 0.15 0.8 0.55]);
hold on
l1=line(Tsim1,Qsim1,'LineWidth',lwd,'color',[0.25, 0.25, 0.25]);
l2=line(Tsim2,Qsim2,'LineWidth',lwd-1,'color',[0.9290, 0.6940, 0.1250]);
xlabel('Time (d)');
ylabel('Streamflow (m³/s)')
legend( [l1;l2;l3] , 'w greenhouse','w/o greenhouse','Atmbc','Location','west','Box','off','Color','none', 'FontName','Calibri','FontSize',30);
set(gca,'FontSize',30)
set(gca,'FontName','Calibri')
% Create postscript outputfile
FILENAME = ['hgraph_compare_atm_Q_split'];
print('-djpeg','-r300',FILENAME);

close all
clear all
HG1=importdata('../output/hgraph',' ',2);

Tsim1=HG1.data(:,1)./86400;
Tsim1_pos=datenum(2023,6,13,0,0,0)+HG1.data(:,1)./86400;
[C,IA,IC] = unique(Tsim1);

Qsim1=HG1.data(IA,2); % could be second or third column
if (sum(Qsim1)>0)
    fprintf('reading column 2')
else
    fprintf('reading column 3')
    Qsim1=HG1.data(IA,3); % could be second or third column
end

% Qsim1=interp1(Tsim(IA),HG1.data(IA,2),T1);
% [E2,D2,E1,D1,RMSE,NRMSE,KGE]=efficiency(Qsim',Q1)

plot(Tsim1,Qsim1,'LineWidth',1.5);
hold on
% dateaxis('X',2)
set(gca,'FontSize',14)%,'YScale','log')
% legend('Irrigation','No irrigation','Location','NorthWest')
xtickangle(20)
ylabel('Streamflow (m^3/s)');
FILENAME = ['../output/hgraph'];
print('-djpeg','-r300',FILENAME);

trapz(Tsim1*86400 , Qsim1) % total volume over event in m3
% SWMM is sum([50 , 203 , 354 , 209]) * 10E+6 % litres
% or
sum([50 , 203 , 354 , 209]) * 10E+3 % m3
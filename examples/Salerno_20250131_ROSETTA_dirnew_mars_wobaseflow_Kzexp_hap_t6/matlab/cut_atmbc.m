%close all
% if ishandle(1), close(1), end % this is because I am using together this script with one for Venezia's data calibration which I number from 4 on to check history of changes
% if ishandle(2), close(2), end
% if ishandle(3), close(3), end
clear all

timest = 10916500;
% normal hgraph script
HG1=importdata('../input/atmbc_0',' ',2);

j = find(HG1.data(:,1) >= timest, 1);
b=HG1.data(j:end,:);
b(1:2:end,1) = b(1:2:end,1) - timest - 2300;
b = [0 1 NaN(1,size(b,2)-2);
    b];
writematrix(b,'../input/atmbc_short','Delimiter',' ');
%% normal hgraph script
HG1=importdata('../../Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzexp_hap_t5/output/hgraph',' ',2);

Tsim1=HG1.data(:,1)./86400 - warmupCATHY; % days
%Tsim1_pos=datenum(2023,12,04,23,59,0)+HG1.data(:,1)./86400;
[C,IA,IC] = unique(Tsim1);

Tsim1 = Tsim1(IA);

Qsim1=HG1.data(IA,2); % could be second or third column
if (sum(Qsim1)>0)
    fprintf('reading column 2\n')
else
    fprintf('reading column 3\n')
    Qsim1=HG1.data(IA,3); % could be second or third column
end


%%
figure
hold on
plot(Tnow,Qnow,'-k','LineWidth',3)
plot(Tsim1,Qsim1,'-r','LineWidth',2)
% plot(QSWMM(:,1)-183,QSWMM(:,2))
xlim([-170 -0])

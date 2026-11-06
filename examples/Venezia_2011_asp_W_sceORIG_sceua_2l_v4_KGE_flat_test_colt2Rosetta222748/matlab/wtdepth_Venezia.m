close all
clear all
HG1=importdata('../output/wtdepth',' ',2);

Tsim1=HG1.data(:,1)./86400;
Tsim1_pos=datenum(2009,4,01,0,0,0)+HG1.data(:,1)./86400;
[C,IA,IC] = unique(Tsim1);
Qsim1=HG1.data(IA,2:end); % could be more than 1 column depending on
% Qsim1=interp1(Tsim(IA),HG1.data(IA,2),T1);
% [E2,D2,E1,D1,RMSE,NRMSE,KGE]=efficiency(Qsim',Q1)



if (size(Qsim1,2) > 1)
    % crop upstream
    % crop downstream
    % greenhouse
    mytitle = {'crop upstream', 'crop downstream', 'greenhouse'};
    for i = 1:size(Qsim1,2)
        subplot(2,2,i)
        plot(Tsim1,Qsim1(:,i),'LineWidth',1.5)
        title(mytitle(i))
    end

else

    plot(Tsim1,Qsim1,'LineWidth',1.5);
    hold on
    % dateaxis('X',2)
    set(gca,'FontSize',14)%,'YScale','log')
    % legend('Irrigation','No irrigation','Location','NorthWest')
    xtickangle(20)
    ylabel('Water table depth (m)');

end
FILENAME = ['../output/wtdepth'];
print('-djpeg','-r300',FILENAME);
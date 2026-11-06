function [KGE]=efficiency2(S,O)

% % delete efficiency.m~;
% E2=1-(sum((S-O).^2)/sum((O-mean(O)).^2));
% D2=1-(sum((S-O).^2)/sum((abs(O-mean(O))+abs(S-mean(O))).^2));
% E1=1-(sum(abs(S-O))/sum(abs(O-mean(O))));
% D1=1-(sum(abs(S-O))/sum(abs(O-mean(O))+abs(S-mean(O))));
%RMSE=(mean((S-O).^2))^0.5;
%NRMSE=RMSE/(max(O)-min(O));
%KGE = NRMSE; % avoid changing things in other scripts
%
r=min(min(corr(S,O, 'rows','complete')));
a=std(S,"omitmissing")/std(O,"omitmissing");
b=mean(S,"omitmissing")/mean(O,"omitmissing");
KGE=1-sqrt((r-1)^2+(a-1)^2+(b-1)^2);
%KGE=1-KGE;

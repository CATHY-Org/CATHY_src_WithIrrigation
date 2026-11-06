function [xc,yc]=contouring(DEM,level)
% Written by E. van Loon
% find contours for an image (square contours, not as usual around
% point, but around square elements), and draw these (that is: 
% draw contour lines around regions, consisting of square elements)
% e.g.:
% dem=[0 0 0 1 1 0;0 0 1 1 1 0;0 1 1 1 0 0; 1 1 0 0 0 0;0 1 0 0 0 0];
% [xc,yc]=contouring(dem,1)
if nargin<2
  level=1
end

[r,c] = find(DEM>=level);
p1 = [r c];
p2 = [r c+1];
p3 = [r+1 c+1];
p4 = [r+1 c];
a = [p1;p2;p4;p1];
b = [p2;p3;p3;p4];
[unlin,nr,rep] = munique([a b]);
silin = unlin((rep==1),:);

vecnan = nan.*ones(length(silin),1);
linesx = [silin(:,[1 3]) vecnan]';
linesy = [silin(:,[2 4]) vecnan]';
xc = linesx(:);
yc = linesy(:);

% imagesc(DEM), hold on, 
% plot(yc,xc,'k'); 

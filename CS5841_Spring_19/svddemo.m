clc
clear
close all

X = im2double( imread('cameraman.tif') );

[U,S,V] = svd( X );

d = size(U,2);
Sfrom1 = zeros(d);
Sfromd = zeros(d);
for ii = 1:d
    
    Sfrom1(ii,ii) = S(ii,ii);
    Sfromd(d-ii+1,d-ii+1) = S(d-ii+1,d-ii+1);
    
    Xfrom1 = U * Sfrom1 * V';
    Xfromd = U * Sfromd * V';
    
    subplot(2,3,1)
    imshow(X), title('Original'),colorbar
    subplot(2,3,2)
    imshow(Xfromd), title(['SVs: ' num2str(d-ii+1) ' to ' num2str(d)]),colorbar
    subplot(2,3,3)
    imshow(Xfrom1), title(['SVs: 1 to ' num2str(ii)]),colorbar
    subplot(2,3,5)
    St = zeros(d); St(d-ii+1,d-ii+1) = S(d-ii+1,d-ii+1);
    imagesc(U* St *V'), title(['Rank-1 Matrix for SV# ' num2str(d-ii+1)]),colormap(gray),colorbar
    subplot(2,3,6)
    St = zeros(d); St(ii,ii) = S(ii,ii);
    imagesc((U* St *V')), title(['Rank-1 Matrix for SV# ' num2str(ii)]),colormap(gray),colorbar
    
    drawnow
    if ii<6
        pause
    end
    
    
end
cons = {'fear','anger','sad','happy'};
T = readtable('/Users/zhangyuan/Documents/beta_ts_corr_50ROIs/SubInfo_2018.txt','Delimiter','\t');
age = table2array(T(:,2)); % age
[ages,ind] = sort(age);

load('Community_fear_All.mat')
[x, y] = sort(Partition');



c=1;
%for c=1:length(cons)
    fname = sprintf('Q_%s_759s.mat',cons{c});
    load(fname); 
    
    all_mod_sorted = all_mod(ind,y)'; % 50 roi * 759 subjects
    
    s=1;
    v = all_mod_sorted(:,s);
    
    
   aa = find(x == 1);
   bb = find(x == 2);
   cc = find(x == 3);
   
   [count,val] = hist(v(aa),unique(v(aa)));
   [i,j] = max(count);
   
   
    
%end
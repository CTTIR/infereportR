# fixture (a): two-series line and points with title and labels

    Code
      print(char_grid(fx_lines(), width = 64, height = 20))
    Output
      C5a after injury                                                
                                                                      
      C5a, ng/ml                                                      
         |                                                            
         |           o....o.                                          
      30 +          .       ..                                        
         |         o          ...                                     
      25 +        .              ...                                  
         |        .                 ..                                
      20 +       o *                  .o......                        
         |      .*/ \*\                       .............           
         |      .      \\                                  ......o    
      15 +     o         \*---                                        
         |    .               ------                                  
      10 +   o                      ---*-------------------------*    
         |                                                            
         +---+----------+----------+----------+----------+----------+ 
             0          10         20         30         40         50
                             Hours after admission                    
      / ISS 16-24   . ISS >= 25                                       

# fixture (b): single-series bars share one fill glyph

    Code
      print(x)
    Output
      MODS, %                                 
         |                                    
         |                     #########      
         |                     #########      
      30 +                     #########      
         |                     #########      
         |                     #########      
      20 +                     #########      
         |                     #########      
         |     #########       #########      
      10 +     #########       #########      
         |     #########       #########      
       0 +     #########       #########      
         |                                    
         +---------+---------------+--------- 
              Peak <= 6 h      Peak > 6 h     

# fixture (c): three list-filled bars with a legend

    Code
      print(x)
    Output
      Response, %                                     
         |                                            
      30 +                 /////////                  
         |                 /////////                  
         |                 /////////                  
      20 +                 /////////    =========     
         |                 /////////    =========     
         |                 /////////    =========     
         |    #########    /////////    =========     
      10 +    #########    /////////    =========     
         |    #########    /////////    =========     
         |    #########    /////////    =========     
       0 +    #########    /////////    =========     
         |                                            
         +--------+------------+------------+-------- 
               Control     High dose     Low dose     
                              Arm                     
      # Control   / High dose   = Low dose            

# fixture (d): error bars and means with a reference line

    Code
      print(char_grid(fx_means(), width = 48, height = 16))
    Output
      Mean (95% CI)                                   
        |                                  ---        
        |                                   |         
      7 +                                   |         
        |                                   *         
      6 +                    ----           |         
        |                      |            |         
      5 +----------------------*--------------------- 
        |        |             |                      
      4 +        *           ----                     
        |        |                                    
        |       ---                                   
      3 +                                             
        +--------+-------------+------------+-------- 
                 A             B            C         
                            Group                     


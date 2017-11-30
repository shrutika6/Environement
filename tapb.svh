// Generated UVM Template -- Created on 2017-03-06 15:08:42.403331324 -0800 PST
//    input file tapb.uv

package cant ;


import uvm_pkg::*;

`include "cant_defs.svh"

`protect
`uvm_analysis_imp_decl(_drivedin)
`uvm_analysis_imp_decl(_regvals)
`uvm_analysis_imp_decl(_expFrame)
`uvm_analysis_imp_decl(_startbit)



// class seq1 template
class seq1 extends uvm_sequence #(Si) ;
  `uvm_object_utils(seq1)

// init code

  Si r;



   function new(string name="seq1");
      super.new(name);
   endfunction : new


// A sequence body template. put tests there
   task body;


  `uvm_info("prelimary","This is a preliminary test bench",UVM_LOW)
  r=Si::type_id::create("td");
  start_item(r);
  r.randomize();
  r.do_reset=1;
  finish_item(r);
  start_item(r);
  r.do_reset=0;
  r.randomize() with { quantaDiv==4; propQuanta==3; seg1Quanta==6;
      datalen < 9; datalen>0; format==0;
      frameType==cantidef::XMITdataframe; id==29'h1000000;};
  finish_item(r);
  repeat(5) begin
    start_item(r);
    r.do_reset=0;
    r.randomize() with { quantaDiv==4; propQuanta==3;
        seg1Quanta<=6; seg1Quanta>0;
      datalen < 9; datalen>0; format==0;
      };
      r.frameType=cantidef::XMITdataframe;
    finish_item(r);
  end
  repeat(5) begin
    start_item(r);
    r.do_reset=0;
    r.randomize() with { quantaDiv>0 && quantaDiv<6;
        propQuanta > 0 && propQuanta<8;
        seg1Quanta<=6; seg1Quanta>0;
      datalen < 9; datalen>=0; format==0;
      };
      r.frameType=cantidef::XMITremoteframe;
    finish_item(r);
  end
  repeat(5) begin
    start_item(r);
    r.do_reset=0;
    r.randomize() with { quantaDiv==4; propQuanta==3;
        seg1Quanta<=6; seg1Quanta>0;
      datalen < 9; datalen>0; format==1;
      };
      r.frameType=cantidef::XMITdataframe;
    finish_item(r);
  end
  repeat(5) begin
    start_item(r);
    r.do_reset=0;
    r.randomize() with { quantaDiv>0 && quantaDiv<6;
        propQuanta > 0 && propQuanta<8;
        seg1Quanta<=6; seg1Quanta>0;
      datalen < 9; datalen>=0; format==1;
      };
      r.frameType=cantidef::XMITremoteframe;
    finish_item(r);
  end

   endtask : body
endclass : seq1


// class inmon template
class inmon extends uvm_monitor ;
  `uvm_component_utils(inmon)

   uvm_analysis_port #(Si) regvals;
   virtual cantintf ci;
// init code

    Si req;



   function new(string name="inmon",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     regvals= new("regvals",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
      if (!uvm_config_db #(virtual cantintf)::get(this, "*","cantintf", ci)) begin
         `uvm_error("connect", "failed to find interface cantintf in DB")
      end
   endfunction : connect_phase


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

    @(posedge(ci.clk)) begin
      if(ci.rst==0 && ci.startXmit==1) begin
        req = new("regs");
        req.quantaDiv = ci.quantaDiv;
        req.propQuanta=ci.propQuanta;
        req.seg1Quanta=ci.seg1Quanta;
        req.xmitdata=ci.xmitdata;
        req.datalen=ci.datalen;
        req.id = ci.id;
        req.format = ci.format;
        req.frameType=ci.frameType;
        regvals.write(req);
      end
    end


         end
      join_none
   endtask : run_phase
endclass : inmon


// class datamon template
class datamon extends uvm_monitor ;
  `uvm_component_utils(datamon)

   uvm_analysis_port #(DBIT) dbit;
   uvm_analysis_imp_drivedin #(reg,datamon) drivedin;
   virtual cantintf ci;
// init code

DBIT req;
reg dother;
reg ddut;



   function new(string name="datamon",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     dbit= new("dbit",this);
     drivedin= new("drivedin",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
      if (!uvm_config_db #(virtual cantintf)::get(this, "*","cantintf", ci)) begin
         `uvm_error("connect", "failed to find interface cantintf in DB")
      end
   endfunction : connect_phase

   // Write function for message drivedin
   function void write_drivedin(input reg din);


  dother=din;
  ci.din <= dother & ddut;

   endfunction : write_drivedin


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

    @(posedge(ci.clk)) begin
      if(ci.rst != 1) begin
          req = new("bitmsg");
          req.dout = ci.dout;
          ddut = ci.dout;
          req.ddrive= ci.ddrive;
          ci.din <= ddut & dother;
          req.din = ci.din;
          dbit.write(req);
      end
    end


         end
      join_none
   endtask : run_phase
endclass : datamon


// class pfind template
class pfind extends uvm_scoreboard ;
  `uvm_component_utils(pfind)

   uvm_tlm_analysis_fifo #(DBIT) dbit;
   uvm_analysis_imp_regvals #(Si,pfind) regvals;
   uvm_analysis_port #(reg) rbit;
   uvm_analysis_port #(reg) startbit;
// init code

  typedef enum int { Bidle,Bedge,Bpost } bstate;
  bstate svar;
  DBIT db;
  reg oldval;
  reg fell,rose,oldbit;
  int oldcnt;
  Si regsi;
  int cntr;
  logic lastval;



   function new(string name="pfind",uvm_component par=null);
     super.new(name,par);
// included new code


svar = Bidle;
oldval=1;
oldcnt=0;
lastval=0;


   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     dbit= new("dbit",this);
     regvals= new("regvals",this);
     rbit= new("rbit",this);
     startbit= new("startbit",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase

   // Write function for message regvals
   function void write_regvals(input Si din);


  regsi=din;

   endfunction : write_regvals


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

  dbit.get(db);
//  `uvm_info("debug",$sformatf("s %h st %d t %f",db.dout,svar,$realtime),UVM_LOW)
  if(db.dout == 0 && oldval==1) fell=1; else fell=0;
  if(db.dout == 1 && oldval==0) rose=1; else rose=0;
  case(svar)
    Bidle: begin
      oldcnt=1;
      if(fell) begin
        svar = Bedge;
        cntr = regsi.quantaDiv;
        cntr *= (1+regsi.propQuanta+regsi.seg1Quanta);
        
      end
    end
    
    Bedge: begin
      if(cntr <= 1) begin
         cntr = regsi.quantaDiv;
         cntr *= (regsi.seg1Quanta);
         svar = Bpost;
         if(db.dout == lastval) begin
            oldcnt=oldcnt+1;
            if(oldcnt > 5 && db.dout==1) begin
            svar = Bidle;    
            end
         end else begin
            oldcnt=1;            
         end
         lastval = db.dout;
//         `uvm_info("debug",$sformatf("sample %h at %f ns %d",db.dout,$realtime,svar),UVM_LOW)
         rbit.write(db.dout);
      end else begin
         cntr=cntr-1; 
      end
    end
    
    Bpost: begin
      if(cntr <= 1) begin
          svar = Bedge;
          cntr = regsi.quantaDiv;
          cntr *= (1+regsi.propQuanta+regsi.seg1Quanta);
          startbit.write(1'b1);
      end else begin
         cntr = cntr-1;
      end
    end
    
  endcase
  oldval = db.dout;

         end
      join_none
   endtask : run_phase
endclass : pfind


// class chkframe template
class chkframe extends uvm_scoreboard ;
  `uvm_component_utils(chkframe)

   uvm_tlm_analysis_fifo #(reg) rbit;
   uvm_analysis_imp_startbit #(reg,chkframe) startbit;
   uvm_analysis_imp_expFrame #(EXPframe,chkframe) expFrame;
   uvm_analysis_port #(reg) drivedin;
// init code

  EXPframe e;
  int fpnt;
  reg rb;
  string ohmy;



   function new(string name="chkframe",uvm_component par=null);
     super.new(name,par);
// included new code

    set_report_max_quit_count(5);

   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     startbit= new("startbit",this);
     expFrame= new("expFrame",this);
     drivedin= new("drivedin",this);
     rbit= new("rbit",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase

   // Write function for message startbit
   function void write_startbit(input reg din);


  if (e.fdata[fpnt]==DA) begin
    drivedin.write(1'b0);  
  end else begin
    drivedin.write(1'b1);
  end

   endfunction : write_startbit

   // Write function for message expFrame
   function void write_expFrame(input EXPframe din);


    e=din;
    fpnt=0;

   endfunction : write_expFrame


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

  rbit.get(rb);
//  `uvm_info("debug",$sformatf("Got %h",rb),UVM_LOW)
  if(e.fdata[fpnt] < 2 && rb !== e.fdata[fpnt]) begin
     `uvm_error("error",$sformatf("Expecting a %h, got  %h",e.fdata[fpnt],rb))
     ohmy="";
     for(int ix=0; ix < e.flen; ix=ix+1) begin
         if(ix != fpnt) begin
           ohmy={ohmy,($sformatf("%d %s\n",e.fdata[ix],e.dname[ix]))};
         end else begin
           ohmy={ohmy,$sformatf("--->(%d %s)<---\n",e.fdata[ix],e.dname[ix])};
         end
     end
     `uvm_info("debug",ohmy,UVM_LOW)
  end
  fpnt=fpnt+1;
  if(fpnt >= e.flen) fpnt=e.flen-1;

         end
      join_none
   endtask : run_phase
endclass : chkframe


// class checkregrw template
class checkregrw extends uvm_scoreboard ;
  `uvm_component_utils(checkregrw)

   uvm_tlm_analysis_fifo #(Ri) rreg;
// init code

Ri r;
reg [31:0] regs[0:3];



   function new(string name="checkregrw",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     rreg= new("rreg",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

    rreg.get(r);
    if(r.write) begin
      regs[r.addr]=r.value;  
    end else begin
      if(r.value !== regs[r.addr]) begin
         `uvm_error("error",$sformatf("Error reading register %d, got %08h expected %08h",
                                      r.addr,r.value,regs[r.addr]));
      end
    end


         end
      join_none
   endtask : run_phase
endclass : checkregrw


// class seqr1 template
class seqr1 extends uvm_sequencer #(Si) ;
  `uvm_component_utils(seqr1)



   function new(string name="seqr1",uvm_component par=null);
     super.new(name,par);
   endfunction : new
endclass : seqr1


// class armon template
class armon extends uvm_monitor ;
  `uvm_component_utils(armon)

   uvm_analysis_port #(Ri) rreg;
   virtual apbintf ai;
// init code

Ri r;



   function new(string name="armon",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     rreg= new("rreg",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
      if (!uvm_config_db #(virtual apbintf)::get(this, "*","apbintf", ai)) begin
         `uvm_error("connect", "failed to find interface apbintf in DB")
      end
   endfunction : connect_phase


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

  @(posedge(ai.PCLK));
  if(ai.PSEL && ai.PENABLE && ai.PREADY && ai.PADDR[4]==0) begin
     r = new();
     r.write = ai.PWRITE;
     r.addr = ai.PADDR[3:2];
     if(r.write) begin
         r.value = ai.PWDATA;
     end else begin
         r.value = ai.PRDATA;
     end
     rreg.write(r);
  end


         end
      join_none
   endtask : run_phase
endclass : armon


// class expframe template
class expframe extends uvm_scoreboard ;
  `uvm_component_utils(expframe)

   uvm_analysis_port #(EXPframe) expFrame;
   uvm_analysis_imp_regvals #(Si,expframe) regvals;
// init code

  Si d;
  EXPframe e;
  reg [14:0] crc;
  int bscnt;
  reg lbit;
  
  function void calcCrc(reg di);
     reg nxb=di ^ crc[14];
     crc=crc << 1;
     if(nxb) begin
       crc = crc ^ 15'h4599;       
     end
//     `uvm_info("debug",$sformatf("crc %4h di %h",crc,di),UVM_LOW)
  endfunction : calcCrc

  function void addbit(Ebit bt,string bname);
//    `uvm_info("debug",$sformatf("Adding %d cnt %d lbit %d",bt,bscnt,lbit),UVM_LOW)
    e.fdata[e.flen]=bt;
    e.dname[e.flen]=bname;
    e.flen += 1;
  endfunction : addbit

  function void addbin(reg di,string nm,reg docrc=1);
    if(di === lbit) begin
      if(bscnt>=5) begin
         bscnt=1;
         addbit((di==0)?D1:D0,"stf");
         lbit= ~di;
      end else bscnt=bscnt+1;
    end else begin
      bscnt = 1;
      lbit=di;
    end
    addbit((di===1'b1)?D1:D0,nm);
    lbit=di;
    if(docrc) calcCrc(di);
  endfunction : addbin



   function new(string name="expframe",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     expFrame= new("expFrame",this);
     regvals= new("regvals",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase

   // Write function for message regvals
   function void write_regvals(input Si din);


    d=din;
    e= new();
    e.flen=0;
    crc=0;
    bscnt=1;
    lbit=1'b1;
    case(d.frameType)
      XMITdataframe,XMITremoteframe: begin
        addbin(0,"sof");
        for( int ix = 28; ix >=18; ix=ix-1) begin
            addbin(d.id[ix],$sformatf("id%02d",ix));
        end
        if(d.format) begin // extended frame
            addbin(1,"srr");
            addbin(0,"ide");       // the extended field (Check this)
            for(int ix = 17; ix >= 0; ix=ix-1) addbin(d.id[ix],$sformatf("id%02d",ix));
        end
        addbin( (d.frameType==XMITremoteframe)?1:0,"rtr" );
        addbin(0,"r1");
        addbin(0,"r0");
        for(int ix=3; ix >=0; ix=ix-1) begin
           addbin(d.datalen[ix],$sformatf("dlen%02d",ix)); 
        end
        if( d.frameType == XMITdataframe) begin
          for(int ix=8; ix > 8-d.datalen; ix=ix-1) begin
            for(int iy=1; iy < 9; iy=iy+1) begin
               addbin(d.xmitdata[ix*8-iy],$sformatf("dbit%02d",ix*8-iy)); 
            end
          end
        end
        for(int ix=14; ix >=0; ix=ix-1) begin
           addbin(crc[ix],$sformatf("crc%02d",ix),0); 
        end
        addbit(DA,"ack");
        addbit(D1,"ack delim");
        for(int ix=7; ix > 0; ix=ix-1) addbit(D1,"eof");
      end
      XMITerrorframe,XMIToverloadframe: begin
        for(int ix=0; ix < 6; ix=ix+1) addbit(D0,"err");
      end
    endcase
    expFrame.write(e);
    

   endfunction : write_regvals


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
//      fork
//         forever begin
//           Needs some form of waiting statement here
//         end
//      join_none
   endtask : run_phase
endclass : expframe


// class bittime template
class bittime extends uvm_scoreboard ;
  `uvm_component_utils(bittime)

   uvm_tlm_analysis_fifo #(DBIT) dbit;
   uvm_analysis_imp_regvals #(Si,bittime) regvals;
// init code


DBIT db;
Si regsi;
reg oldval;
int bcnt;
int modres;
int bsize;



   function new(string name="bittime",uvm_component par=null);
     super.new(name,par);
// included new code

  regsi=null;
  oldval=1'bX;
  bcnt=0;

   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
     dbit= new("dbit",this);
     regvals= new("regvals",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase

   // Write function for message regvals
   function void write_regvals(input Si din);


  regsi=din;
  bsize = (1+regsi.propQuanta+2*regsi.seg1Quanta)*regsi.quantaDiv;

   endfunction : write_regvals


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

    dbit.get(db);
    if(db.dout===0'b1 && oldval === 1'b1) begin
        bcnt=0;
    end else if(db.dout===1'b1 && oldval === 1'b0) begin
        modres = bcnt % bsize;
        if(modres !=0) begin
           `uvm_error("error",$sformatf("bit time error. Remainder of %d is %d clocks",bsize,modres)) 
        end
        bcnt = 0;
    end else begin
        bcnt += 1;
//        `uvm_info("bit",$sformatf("bit %h bcnt %d",db.dout,bcnt),UVM_LOW)
    end
    oldval = db.dout;

         end
      join_none
   endtask : run_phase
endclass : bittime


// class drv1 template
class drv1 extends uvm_driver #(Si) ;
  `uvm_component_utils(drv1)

   virtual cantintf ci;
   virtual apbintf ai;
// init code

    Si req;
    int deathcount;
    reg [31:0] dbase;
    reg [31:0] busy;
    reg [31:0] wv,checkval;
    
    task writereg(input reg[31:0] addr,input [31:0] dw);
      ai.PADDR <= addr;
      ai.PWDATA<=dw;
      ai.PENABLE<=0;
      ai.PWRITE<=1;
      if( (addr&32'hFFFF_FF00)==32'hf000_ff00) begin
        ai.PSEL<=1;
      end
      @(posedge(ai.PCLK)) #1;
      ai.PENABLE<=1;
      @(posedge(ai.PCLK));
      while(ai.PREADY==0) @(posedge(ai.PCLK));
      #1 ai.PSEL<=0;
      ai.PENABLE<=0;
      ai.PADDR<=32'ha5a5a5a5;
      ai.PWDATA<=32'h12345678;
    endtask : writereg
    
    task readreg(input reg[31:0] addr, output [31:0] rdata);
      ai.PADDR <= addr;
      ai.PWDATA<=32'h87654321;
      ai.PENABLE<=0;
      ai.PWRITE<=0;
      if( (addr&32'hFFFF_FF00)==32'hf000_ff00) begin
        ai.PSEL<=1;
      end
      @(posedge(ai.PCLK)) #1;
      ai.PENABLE<=1;
      @(posedge(ai.PCLK));
      while(ai.PREADY==0) @(posedge(ai.PCLK));
      rdata = ai.PRDATA;
      #1 ai.PSEL<=0;
      ai.PENABLE<=0;
      ai.PADDR<=32'ha5a5a5a5;
      ai.PWDATA<=32'h12345678;
    
    
    endtask : readreg



   function new(string name="drv1",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
      if (!uvm_config_db #(virtual apbintf)::get(this, "*","apbintf", ai)) begin
         `uvm_error("connect", "failed to find interface apbintf in DB")
      end
      if (!uvm_config_db #(virtual cantintf)::get(this, "*","cantintf", ci)) begin
         `uvm_error("connect", "failed to find interface cantintf in DB")
      end
   endfunction : connect_phase


// A run_phase template. Remove the following comments if used
   task run_phase(uvm_phase phase); 
      fork
         forever begin
//           Needs some form of waiting statement here

  // my code here
  seq_item_port.get_next_item(req); // Gets the sequence_item
  dbase=32'hF000_FF00;
  if(req.do_reset) begin
    ci.rst<=1;
    ai.PRESET<=1;
    ai.PSEL<=0;
    ai.PENABLE<=0;
    ai.PADDR<=0;
    ai.PWDATA<=0;
    repeat(3) @(posedge(ai.PCLK)) #1; 
    ci.rst<=0;
    ai.PRESET<=0;
    repeat(4) @(posedge(ai.PCLK)) #1;
  end else begin
    writereg(dbase+4,req.xmitdata[31:0]);
    writereg(dbase,req.xmitdata[63:32]);
    writereg(dbase+8,{req.quantaDiv,req.propQuanta,req.seg1Quanta,
        req.datalen,req.format,req.frameType,5'b0});
    writereg(dbase+12,{req.id,3'b0});
    writereg(dbase+16,$random);
    repeat(3) @(posedge(ai.PCLK)) #1;
    deathcount=(120+req.datalen*9)*(req.quantaDiv)*(1+req.propQuanta+2*req.seg1Quanta)/2;
    busy=1;
    while(deathcount > -1000 && busy[0] === 1) begin
        wv=$urandom_range(0,4);
        if(wv==4) begin
            readreg(dbase+16,busy);
        end else begin
            readreg(dbase+(4*wv),checkval);
        end
//        readreg(dbase+16,busy);
        deathcount -= 1;
    end
    if(busy[0] === 1'bx) begin
       `uvm_error("error","busy reads back as 'X'") 
    end

    if(deathcount <= 0) begin
       ci.oops<= 1;
       `uvm_error("error","Ran out of clocks for message") 
    end
  end
  seq_item_port.item_done();

         end
      join_none
   endtask : run_phase
endclass : drv1


// class agent1 template
class agent1 extends uvm_agent ;
  `uvm_component_utils(agent1)

   checkregrw crrw ;
   seqr1 sqr1 ;
   inmon imon ;
   datamon dmon ;
   pfind pf ;
   expframe expf ;
   chkframe cframe ;
   drv1 d1 ;
   bittime btime ;
   armon armx ;


   function new(string name="agent1",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
      dmon = datamon::type_id::create("dmon",this);
      pf = pfind::type_id::create("pf",this);
      expf = expframe::type_id::create("expf",this);
      cframe = chkframe::type_id::create("cframe",this);
      crrw = checkregrw::type_id::create("crrw",this);
      sqr1 = seqr1::type_id::create("sqr1",this);
      imon = inmon::type_id::create("imon",this);
      armx = armon::type_id::create("armx",this);
      d1 = drv1::type_id::create("d1",this);
      btime = bittime::type_id::create("btime",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
      imon.regvals.connect(pf.regvals);
      imon.regvals.connect(expf.regvals);
      imon.regvals.connect(btime.regvals);
      dmon.dbit.connect(pf.dbit.analysis_export);
      dmon.dbit.connect(btime.dbit.analysis_export);
      cframe.drivedin.connect(dmon.drivedin);
      pf.rbit.connect(cframe.rbit.analysis_export);
      pf.startbit.connect(cframe.startbit);
      expf.expFrame.connect(cframe.expFrame);
      armx.rreg.connect(crrw.rreg.analysis_export);
      d1.seq_item_port.connect(sqr1.seq_item_export);
   endfunction : connect_phase
endclass : agent1


// class env1 template
class env1 extends uvm_env ;
  `uvm_component_utils(env1)

   agent1 a1 ;


   function new(string name="env1",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
      a1 = agent1::type_id::create("a1",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase
endclass : env1


// class t1 template
class t1 extends uvm_test ;
  `uvm_component_utils(t1)

   env1 e1 ;
   seq1 s1 ;


   function new(string name="t1",uvm_component par=null);
     super.new(name,par);
   endfunction : new


//  The build phase is to create any components or other
//  elements required
   function void build_phase(uvm_phase phase);
     super.build_phase(phase);
      e1 = env1::type_id::create("e1",this);
      s1 = seq1::type_id::create("s1",this);
   endfunction : build_phase


//  The connect phase is to bind messages and interfaces
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase


// The test run_phase starts the tests
   task run_phase(uvm_phase phase);
      // Keep simulation running by raising the objection
      phase.raise_objection(this,"start sequence");
//      Change sequence order as needed
      s1.start(e1.a1.sqr1);
      phase.drop_objection(this,"end sequence");
   endtask : run_phase
endclass : t1

`endprotect

endpackage : cant

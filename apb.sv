// A simple and fun apb interface to the can transmitter
//


module apb(apbintf.slv c, cantintf.tox a);
    typedef struct packed {
        reg [7:0] quantaDiv;
        reg [5:0] propQuanta,seg1Quanta;
        reg [3:0] datalen;
        reg       format;
        reg [1:0] frameType;
        reg [4:0] Reserved;
    } cmdT;

    typedef struct packed {
        reg [28:0] id;
        reg [2:0] Reserved;
    } idT;
    
    cmdT cmd;
    idT id;

    reg [63:0] dataword;
    reg startit;
    
    always @(*) begin
      a.startXmit=startit;
      a.quantaDiv = cmd.quantaDiv;
      a.propQuanta = cmd.propQuanta;
      a.seg1Quanta = cmd.seg1Quanta;
      a.datalen = cmd.datalen;
      a.format = cmd.format;
      a.id=id.id;
      a.frameType = cmd.frameType;
      a.xmitdata = dataword;
    end
    
    always @(*) begin
      if(c.PENABLE && c.PSEL && (c.PADDR[4:0]==16) && (c.PWRITE==1'b1) ) begin
        startit=1;
      end else begin
        startit=0;
      end
    
    end
    
    always@(*) begin
      if(c.PENABLE && c.PSEL && c.PWRITE==1'b0) begin
        case(c.PADDR[4:0])
          0: c.PRDATA=dataword[63:32];
          4: c.PRDATA=dataword[31:0];
          8: c.PRDATA=cmd;
          12: c.PRDATA=id;
          16: c.PRDATA= {31'b0,a.busy};
        endcase
      end else c.PRDATA=32'h12131415;
    end
    
    always @(posedge(c.PCLK)) begin
        if(c.PRESET) begin
            dataword <= 0;
            id <= 0;
            cmd <= 0;   
        end else begin
            if(c.PENABLE && c.PSEL && c.PWRITE ) begin
                case(c.PADDR[4:0])
                   0: dataword[63:32]<=#1 c.PWDATA;
                   4: dataword[31:0]<=#1 c.PWDATA;
                   8: cmd <= #1 c.PWDATA&32'hFFFF_FFE0;
                  12: id <= #1 c.PWDATA&32'hFFFF_FFF8;
                endcase
            end
        end
    end





endmodule : apb

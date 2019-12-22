(function(){
    YAHOO.widget.Chart.SWFURL = "http://yui.yahooapis.com/2.6.0/build//charts/assets/charts.swf";

    function Mortgage(principalAmount, termInYears, interestRate){
        this.principalAmount = principalAmount;
        this.numPayments = termInYears * 12.0;
        this.monthlyRate = interestRate / 12.0;
        
        this.calculatePayment = function(){
            var monthlyPayment = this.principalAmount *
                (this.monthlyRate / (1 - Math.pow(1 + this.monthlyRate, -this.numPayments)));
            return monthlyPayment;
        }
        
        this.getPayments = function(){
            var payments = [];
            var payment = this.calculatePayment();

            var currentPrincipal = this.principalAmount;
            for (var month = 0; month < this.numPayments; month++) {
                var interestPayment = this.monthlyRate * currentPrincipal;
                var principalPayment = payment - interestPayment;

                currentPrincipal = Math.abs(currentPrincipal - principalPayment);

                payments[month] = [month + 1, principalPayment, interestPayment, currentPrincipal];
            }
            
            return payments;
        }
    }

    var formatCurrencyAxisLabel = function(value) {
        return YAHOO.util.Number.format(value, {
            prefix: "$",
            thousandsSeparator: ",",
            decimalPlaces: 2
        });
    };

    getDataTipText = function(item, index, series) {
        var toolTipText = "Month #" + item.month;
        toolTipText += "\n" + formatCurrencyAxisLabel(item[series.yField])
                    + " " + series.displayName;
        return toolTipText;
    }

    var calculateButton = document.getElementById("calc-button");
    var principalAmount = document.getElementById("principal-amount");
    var termInYears = document.getElementById("term");
    var interestRate = document.getElementById("interest-rate");
    
    var onCalculate = function(event){
        YAHOO.util.Event.stopEvent(event);

        var p = parseFloat(principalAmount.value);
        var t = parseInt(termInYears.value);
        var r = parseFloat(interestRate.value / 100.0);

        var mortgage = new Mortgage(p, t, r);

        var payments = mortgage.getPayments();

        var myColumnDefs = [{
            key: 'month',
            label: 'Payment #'
        }, {
            key: 'principal',
            label: 'Principal',
            formatter: "currency"
        }, {
            key: 'interest',
            label: 'Interest',
            formatter: "currency"
        }, {
            key: 'remaining',
            label: 'Remaining Balance',
            formatter: "currency"
        }];

        var myDataSource = new YAHOO.util.DataSource(payments);
        myDataSource.responseType = YAHOO.util.DataSource.TYPE_JSARRAY;
        myDataSource.responseSchema = {
            fields: ["month", "principal", "interest", "remaining"]  
        };

        var myDataTable = new YAHOO.widget.DataTable("amortization-table", 
                                                     myColumnDefs,
                                                     myDataSource, {
                                                        scrollable: true,
                                                        width: "400px",
                                                        height: "350px"
                                                     });

        var seriesDef = [
            { displayName: "Interest", yField: "interest" },
            { displayName: "Principal", yField: "principal" }
        ];

        var monthAxis = new YAHOO.widget.NumericAxis();
        monthAxis.minimum = 1;
        monthAxis.majorUnit = 50;

        var currencyAxis = new YAHOO.widget.NumericAxis();
        currencyAxis.minimum = 0;
        currencyAxis.labelFunction = formatCurrencyAxisLabel;

        var myDataChart = new YAHOO.widget.LineChart("amortization-graph", 
            myDataSource, {
                series: seriesDef,
                xField: "month",
                xAxis: monthAxis,
                yAxis: currencyAxis,
                dataTipFunction: getDataTipText
            }
        );
    };

    YAHOO.util.Event.addListener(calculateButton, "click", onCalculate);
})();

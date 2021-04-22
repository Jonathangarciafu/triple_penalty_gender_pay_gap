capture program drop xttrans2
program define xttrans2, rclass byable(recall) sort
	version 6, missing
	syntax varname [if] [in] [, Freq I(varname) T(varname)]
	xt_iis `i'
	local ivar "`s(ivar)'"

	xt_tis `t'
	local tvar "`s(timevar)'"

	if "`freq'"!="" {
			local opts "row freq"
	}
	else    local opts "row nofreq"

	tempvar touse
	mark `touse' `if' `in'
	markout `touse' `varlist' `ivar' `tvar'

	tempvar was is
	quietly {
			sort `ivar' `tvar'
			by `ivar': gen float `was' = `varlist' if _n<_N
			by `ivar': gen float `is'  = `varlist'[_n+1] if _n<_N
			local lbl : var label `varlist'
			if "`lbl'"=="" {
					local lbl "`varlist'"
			}
			label var `was' "`lbl'"  
			label var `is' "`lbl'"
			by `ivar': replace `touse'=0 if `touse'[_n+1]==0 & _n<_N
	}
	tempname table row col
	tabulate `was' `is' if `touse', `opts' nokey ///
		matcell(`table') matrow(`row') matcol(`col')
	ret add

    if "`freq'" == "" {
        // table contains frequencies
        // turn that into row percentages
        mata st_matrix(st_local("table"),st_matrix(st_local("table")):/rowsum(st_matrix(st_local("table"))):*100)
    }

    // add stripes
    local lbl : subinstr local lbl " " "_", all
    // rownames
    forvalues i = 1/`=rowsof(`table')' {
        local vlbl : label (`varlist') `=el(`row',`i',1)'
        local vlbl : subinstr local vlbl " " "_", all
        local rnames `"`rnames' `lbl':`vlbl'"'
    }
    // colnames
    forvalues i = 1/`=colsof(`table')' {
        local vlbl : label (`varlist') `=el(`col',1,`i')'
        local vlbl : subinstr local vlbl " " "_", all
        local cnames `"`cnames' `lbl':`vlbl'"'
    }
    matrix rownames `table' = `rnames'
    matrix colnames `table' = `cnames'

    // return matrix
    return matrix table = `table'
end
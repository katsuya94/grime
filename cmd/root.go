package cmd

import (
	"fmt"
	"os"

	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
	_fmt "github.com/katsuya94/grime/lib/fmt"
	"github.com/katsuya94/grime/lib/grime"
	"github.com/katsuya94/grime/runtime"
	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "grime [filename]",
	Short: "Grime is a programming language for developing joyful developer experiences that wrap Go",
	Long:  "Grime is a programming language for developing joyful developer experiences that wrap Go. Complete documentation is available at https://github.com/katsuya94/grime",
	Args:  cobra.RangeArgs(0, 1),
	RunE: func(cmd *cobra.Command, args []string) error {
		if len(args) == 0 {
			return replCmd.RunE(cmd, []string{})
		}
		return runCmd.RunE(cmd, args)
	},
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func newRuntime() (*runtime.Runtime, error) {
	rt := runtime.NewRuntime(core.Compile)
	err := rt.Provide(core.Library)
	if err != nil {
		return nil, err
	}
	err = rt.Bind(core.Library.Name(), core.Bindings)
	if err != nil {
		return nil, err
	}
	err = rt.Provide(_fmt.Library)
	if err != nil {
		return nil, err
	}
	err = rt.Bind(_fmt.Library.Name(), _fmt.Bindings)
	if err != nil {
		return nil, err
	}
	err = rt.Provide(derived.Library)
	if err != nil {
		return nil, err
	}
	err = rt.Provide(base.Library)
	if err != nil {
		return nil, err
	}
	err = rt.Provide(grime.Library)
	if err != nil {
		return nil, err
	}
	return rt, nil
}

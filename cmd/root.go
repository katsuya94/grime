package cmd

import (
	"fmt"
	"os"

	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
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

func newRuntime() *runtime.Runtime {
	rt := runtime.NewRuntime()
	rt.Provide(core.Library)
	rt.Bind(core.Library.Name(), core.Bindings)
	rt.Provide(derived.Library)
	rt.Provide(base.Library)
	rt.Provide(grime.Library)
	return rt
}

package cmd

import (
	"fmt"
	"os"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/runtime"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(replCmd)
}

var libraryNames = [][]common.Symbol{
	{common.Symbol("grime")},
	{common.Symbol("fmt")},
}

var replCmd = &cobra.Command{
	Use:   "repl",
	Short: "Start an interactive REPL",
	Args:  cobra.NoArgs,
	Run: func(cmd *cobra.Command, args []string) {
		err := repl()
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	},
}

func repl() error {
	rt, err := newRuntime()
	if err != nil {
		return err
	}
	err = rt.Instantiate([]common.Symbol{common.Symbol("grime")})
	if err != nil {
		return err
	}
	bindings := common.NewBindingSet()
	for _, name := range libraryNames {
		b, err := rt.BindingsFor(name)
		if err != nil {
			return err
		}
		bindings.Merge(b)
	}
	runtime.REPL(core.Compile, bindings, os.Stdin, os.Stdout)
	return nil
}

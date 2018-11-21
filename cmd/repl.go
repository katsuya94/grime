package cmd

import (
	"os"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/runtime"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(replCmd)
}

var replCmd = &cobra.Command{
	Use:   "repl",
	Short: "Start an interactive REPL",
	Args:  cobra.NoArgs,
	RunE: func(cmd *cobra.Command, args []string) error {
		rt := newRuntime()
		rt.Instantiate([]common.Symbol{common.Symbol("grime")})
		bindings, err := rt.BindingsFor([]common.Symbol{common.Symbol("grime")})
		if err != nil {
			return err
		}
		runtime.REPL(bindings, os.Stdin, os.Stdout)
		return nil
	},
}

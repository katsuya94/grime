package cmd

import (
	"fmt"
	"os"

	"github.com/katsuya94/grime/lib"
	"github.com/katsuya94/grime/lib/core"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(replCmd)
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
	err := lib.Runtime.Instantiate(lib.StandardLibraryName)
	if err != nil {
		return err
	}
	bindings := lib.Runtime.BindingsFor(lib.StandardLibraryName)
	lib.Runtime.REPL(core.Compile, bindings, os.Stdin, os.Stdout)
	return nil
}

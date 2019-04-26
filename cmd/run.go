package cmd

import (
	"fmt"
	"os"

	"github.com/katsuya94/grime/lib"
	"github.com/katsuya94/grime/read"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(runCmd)
}

var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Run a Grime top-level program",
	Args:  cobra.ExactArgs(1),
	Run: func(cmd *cobra.Command, args []string) {
		err := run(args[0])
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	},
}

func run(path string) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	topLevelProgram, nullSourceLocationTree, err := read.Read(path, f)
	if err != nil {
		return err
	}
	return lib.Runtime.Execute(topLevelProgram, nullSourceLocationTree)
}

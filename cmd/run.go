package cmd

import (
	"fmt"
	"io"
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
	Args:  cobra.RangeArgs(0, 1),
	Run: func(cmd *cobra.Command, args []string) {
		path := ""
		if len(args) == 1 {
			path = args[0]
		}
		err := run(path)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	},
}

func run(path string) error {
	var (
		r   io.Reader = os.Stdin
		err error
	)
	if path != "" {
		r, err = os.Open(path)
		if err != nil {
			return err
		}
	}
	topLevelProgram, nullSourceLocationTree, err := read.Read(path, r)
	if err != nil {
		return err
	}
	return lib.Runtime.Run(topLevelProgram, nullSourceLocationTree)
}

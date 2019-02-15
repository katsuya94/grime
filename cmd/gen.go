package cmd

import (
	"fmt"
	"os"

	"github.com/katsuya94/grime/gen"
	"github.com/spf13/cobra"
)

func init() {
	rootCmd.AddCommand(genCmd)
}

var genCmd = &cobra.Command{
	Use:   "gen",
	Short: "Generate Grime bindings for a given Go package",
	Run: func(cmd *cobra.Command, args []string) {
		for _, arg := range args {
			err := gen.Generate(arg)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
		}
	},
}
